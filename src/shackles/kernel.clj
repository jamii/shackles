(ns shackles.kernel
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [mist.strict :as strict])
  (:import [clojure.lang PersistentQueue]))

;; shorthand:
;;   variable -> var
;;   domain -> dom
;;   subscription -> sub
;;   propagator -> prop
;;   strategy -> strat
;;   search combinator -> comb

(defrecord Var [name id])

(def ^:private next-id (atom 0))

(defn fresh-var [name]
  (Var. name (swap! next-id + 1)))

;; the state of the solver at a given point in time
(defrecord State
    [doms ; {var dom}
     subs ; {var {event-class #{prop}}}
     events ; {prop {var #{event}}}
     agenda ; (priority-map prop priority)
     status]) ; either :ok or :failed

(def empty-state
  (State. {} {} {} (priority-map) :ok))

(defn failed [state]
  (assoc state :status :failed))

(defn failed? [state]
  (= :failed (:status state)))

(defrecord Equal [value])
(defrecord NotEqual [value]) ; often ignored as an action and not generated as an event - too finegrained

(defprotocol Domain ; must handle the Equal and NotEqual actions
  (assigned? [dom] "Has the domain been reduced to a single value?")
  (assigned [dom] "Return the assigned value of the domain, or nil if not assigned")
  (perform [dom action] "Performs the action and returns a modified dom")
  (status [old-dom new-dom action] "Returns either :ok or :failed")
  (events [old-dom new-dom action] "Returns a list of events triggered by the change from old-dom to new-dom"))

(defprotocol Propagator
  (priority [prop] "Priority in agenda (lower number is higher priority")
  (init [prop state] "Initialise the prop. May subscribe to vars, generate events and schedule itself. Returns a modified state.")
  (propagate [prop state vars&events] "Run the prop. May do any of the above and amy apply actions. Returns a modified state"))

(defn add-var [state var dom]
  (merge state
         {:doms (assoc (:doms state) var dom)
          :subs (assoc (:subs state) var {})}))

(defn add-vars [state vars&doms]
  (reduce (fn [state [var dom]] (add-var state var dom)) state vars&doms))

(defn- update-with-default [map keys default f & args]
  (update-in map keys
             (fn [val]
               (let [new-val (if (nil? val) default val)]
                 (apply f new-val args)))))

(defn subscribe-var [state prop var event-class]
  (update-with-default state [:subs var event-class] #{} conj prop))

(defn subscribe-vars [state prop vars&event-classes]
  (reduce (fn [state [var event-class]] (subscribe-var state prop var event-class)) state vars&event-classes))

(defn unsubscribe-var [state prop var event-class]
  (update-with-default state [:subs var event-class] #{} disj prop))

(defn unsubscribe-vars [state prop vars&event-classes]
  (reduce (fn [state [var event-class]] (unsubscribe-var state prop var event-class)) state vars&event-classes))

(defn add-prop [state prop]
  (init prop state))

(defn add-props [state props]
  (reduce add-prop state props))

(defn schedule-prop [state prop]
  (update-in state [:agenda] conj [prop (priority prop)]))

(defn clear-prop [state prop]
  (-> state
      (update-in [:agenda] dissoc prop)
      (update-in [:events] dissoc prop)))

(defn handle-event [state var event]
  (let [props (get-in state [:subs var (class event)])]
    (reduce
     (fn [state prop]
       (let [state (schedule-prop state prop)
             state (update-in state [:events prop var] conj event)]
         state))
     state
     props)))

(defn handle-events [state vars&events]
  (reduce (fn [state [var event]] (handle-event state var event)) state vars&events))

(defn handle-action [state var action]
  (let [old-dom (get-in state [:doms var])
        new-dom (perform old-dom action)
        state (assoc-in state [:doms var] new-dom)
        new-status (status old-dom new-dom action)]
    (strict/match [new-status]
      [:failed] (failed state)
      [:ok] (reduce #(handle-event %1 var %2) state (events old-dom new-dom action)))))

(defn- reduce-until
  ([cond f val coll]
     (if (or (empty? coll) (cond val))
       val
       (recur cond f (f val (first coll)) (rest coll)))))

(defn handle-actions [state vars&actions]
  (reduce-until
   failed?
   (fn [state [var action]]
     (handle-action state var action))
   state
   vars&actions))

(defn select-vars [state vars]
  (select-keys (:doms state) vars))

(defn stable? [state]
  (or (failed? state) (empty? (:agenda state))))

(defn stabilise [state]
  "Apply props from the agenda until the state is stable."
  (if (stable? state)
    state
    (let [agenda (:agenda state)
          [prop _] (peek agenda)
          state (assoc state :agenda (pop agenda))
          vars&events (get-in state [:events prop])
          state (update-in state [:events prop] {})
          state (propagate prop state vars&events)]
      (recur state))))

;; basic domain

(defrecord SimpleDomain [value]
  Domain
  (assigned? [dom]
    (not (nil? value)))
  (assigned [dom]
    value)
  (perform [dom action]
    (strict/match-type action
      [Equal {:value value}] (SimpleDomain. value)
      [NotEqual {:value value}] dom))
  (status [old-dom new-dom action]
    (if (and (not= old-dom new-dom) (assigned? old-dom))
      :failed
      :ok))
  (events [old-dom new-dom action]
    (if (not= old-dom new-dom)
      [(Equal. (:value new-dom))])))

(def any (SimpleDomain. nil))
(defn only [value] (SimpleDomain. value))

;; basic props

(defrecord AllEqual [vars]
  Propagator
  (priority [this]
    0)
  (init [this state]
    (-> state
        (subscribe-vars this (for [var vars] [var Equal]))
        (handle-events (for [[var dom] (select-vars state vars)
                             :when (assigned? dom)]
                         [var (Equal. (assigned dom))]))))
  (propagate [this state vars&events]
    (let [vars&actions (for [[assigned-var events] vars&events
                             event events
                             var vars
                             :when (not (= assigned-var var))]
                         [var (Equal. (:value event))])]
      (-> state
          (handle-actions vars&actions)
          (clear-prop this)))))

(def all-equal ->AllEqual)

(defrecord AllDifferent [vars]
  Propagator
  (priority [this]
    0)
  (init [this state]
    (-> state
        (subscribe-vars this (for [var vars] [var Equal]))
        (handle-events (for [[var dom] (select-vars state vars)
                             :when (assigned? dom)]
                         [var (Equal. (assigned dom))]))))
  (propagate [this state vars&events]
    (let [vars&actions (for [[assigned-var events] vars&events
                             event events
                             var vars
                             :when (not (= assigned-var var))]
                         [var (NotEqual. (:value event))])]
      (-> state
          (handle-actions vars&actions)
          (clear-prop this)))))

(def all-different ->AllDifferent)
