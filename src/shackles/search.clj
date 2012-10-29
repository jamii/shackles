(ns shackles.search
  (:require [shackles.kernel :as kernel])
  (:import [shackles.kernel Equal NotEqual]
           [clojure.lang PersistentQueue]))

(defprotocol Space
  "Defines a search space (starting from a empty state)"
  (children [comb root] "Applies the search combinator to the root space, returning a finite list of (combinator, state) pairs."))

;; core combinators

(defrecord Terminate []
  Space
  (children [_ root] nil))

(def terminate (Terminate.))

(defrecord Expand [vars&doms]
  Space
  (children [_ root]
    (list [terminate (kernel/add-vars root vars&doms)])))

(defrecord Constrain [props]
  Space
  (children [_ root]
    (list [terminate (kernel/stabilise (kernel/add-props root props))])))

(defrecord Until [condition start-comb end-comb]
  Space
  (children [this root]
    (if (condition root)
      (children end-comb root)
      (for [[new-start-comb new-root] (children start-comb root)]
        (let [new-this (if (identical? start-comb new-start-comb)
                     this
                     (Until. condition new-start-comb end-comb))]
          [new-this new-root])))))

(defrecord Or [combs]
  Space
  (children [_ root]
    (mapcat (fn [comb] (children comb root)) combs)))

(def fifo PersistentQueue/EMPTY)
(def filo ())

(defrecord And [combs]
  Space
  (children [_ root]
    (when-first [comb combs]
      (for [[new-comb new-root] (children comb root)]
        [(And. (conj new-comb combs)) new-root]))))

;; nicer interface

(defn or-par [combs] (Or. combs))
(defn and-par [combs] (And. (into fifo combs)))
(defn and-seq [combs] (And. (into filo combs)))

(defmacro let-var [bindings & body]
  (let [new-bindings (for [[var-sym dom] (partition 2 bindings)]
                       (let [dom-sym (gensym (str var-sym "-dom"))]
                         `[~dom-sym ~dom
                           ~var-sym (kernel/fresh-var (quote ~var-sym))]))
        expanders (for [[dom-sym _ var-sym _] new-bindings]
                    `[~var-sym ~dom-sym])]
    `(let [~@(apply concat new-bindings)]
       (and-seq
        [(Expand. ~expanders)
         ~@body]))))

(defmacro let-vars [bindings & body]
  (let [new-bindings (for [[var-sym doms] (partition 2 bindings)]
                       (let [dom-sym (gensym (str var-sym "-dom"))]
                         `[~dom-sym ~doms
                           ~var-sym (for [_# ~dom-sym]
                                      (kernel/fresh-var (quote ~var-sym)))]))
        expanders (for [[dom-sym _ var-sym _] new-bindings]
                    `(map vector ~var-sym ~dom-sym))]
    `(let [~@(apply concat new-bindings)]
       (and-seq
        [(Expand. (apply concat ~(vec expanders)))
         ~@body]))))

;; searching

(defrecord Tree [root children])

(defn expand [[comb root]]
  (let [root (kernel/stabilise root)]
    (Tree.
     root
     (when-not (or (identical? terminate comb) (kernel/failed? root))
       (map expand (children comb root))))))

(defn- search-with-queue [condition queue]
  (if (empty? queue)
    nil
    (let [tree (peek queue)
          root (.root tree)
          children (.children tree)
          queue (pop queue)]
      (if (condition root children)
        (lazy-seq (cons root (search-with-queue condition queue)))
        (recur condition (into queue children))))))

(defn breadth-first [condition comb state]
  (search-with-queue condition (conj fifo (expand [comb state]))))

(defn depth-first [condition comb state]
  (search-with-queue condition (conj filo (expand [comb state]))))

(defn non-failed-leaf? [root children]
  (and (not (kernel/failed? root)) (empty? children)))
