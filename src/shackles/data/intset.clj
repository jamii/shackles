(ns shackles.data.intset
  (:require [mist.strict :as strict])
  (:import [clojure.lang Counted Seqable IPersistentCollection IPersistentSet]
           [java.lang Object UnsupportedOperationException]))

(defprotocol BoundedSet
  (lower-bound [set] "Return the (inclusive) lower bound")
  (upper-bound [set] "Return the (inclusive) upper bound")
  (gte [set bound] "Return the set restricted to elements >= bound")
  (lte [set bound] "Return the set restricted to elements <= bound"))

(declare segment)

(deftype Branch [mid lo hi] ; lo branch is < mid, hi branch is >= mid
  Object
  (equals [this that] ; from Programming Clojure
    (or
     (identical? this that)
     (and (or (instance? java.util.Set that)
              (instance? clojure.lang.IPersistentSet that))
          (= (count this) (count that))
          (every? #(contains? this %) that))))
  (hashCode [this]
    (unchecked-add-int (.hashCode lo) (.hashCode hi)))
  Counted
  (count [this]
    (+ (.count lo) (.count hi)))
  Seqable
  (seq [this]
    (lazy-cat (.seq lo) (.seq hi)))
  IPersistentCollection
  (empty [this]
    (segment 0 -1))
  (equiv [this that]
    (.equals this that))
  (cons [this elem]
    (throw (UnsupportedOperationException. "Branch.cons"))) ; maybe later
  IPersistentSet
  (contains [this elem]
    (if (< elem mid)
      (.contains lo elem)
      (.contains hi elem)))
  (get [this elem]
    (if (< elem mid)
      (.get lo elem)
      (.get hi elem)))
  (disjoin [this elem]
    (if (< elem mid)
      (let [lo (.disjoin lo elem)]
        (if (empty? lo) hi (Branch. mid lo hi)))
      (let [hi (.disjoin hi elem)]
        (if (empty? hi) lo (Branch. mid lo hi)))))
  BoundedSet
  (lower-bound [this]
    (.lower-bound lo))
  (upper-bound [this]
    (.upper-bound hi))
  (gte [this bound]
    (if (< bound mid)
      (let [lo (.gte lo bound)]
        (if (empty? lo) hi (Branch. mid lo hi)))
      (.gte hi bound)))
  (lte [this bound]
    (if (< bound mid)
      (.lte lo bound)
      (let [hi (.lte hi bound)]
        (if (empty? hi) lo (Branch. mid lo hi))))))

(deftype Segment [lo hi] ; bounds are inclusive
  Object
  (equals [this that] ; from Programming Clojure
    (or
     (identical? this that)
     (and (or (instance? java.util.Set that)
              (instance? IPersistentSet that))
          (= (count this) (count that))
          (every? #(contains? this %) that))))
  (hashCode [this]
    (unchecked-add-int lo hi))
  Counted
  (count [this]
    (+ 1 (- hi lo)))
  Seqable
  (seq [this]
    (if (<= lo hi) ; (range 0 0) returns () which means (empty? (segment 0 -1)) returns false :(
      (range lo (+ 1 hi))
      nil))
  IPersistentCollection
  (empty [this]
    (Segment. 0 -1))
  (equiv [this that]
    (.equals this that))
  (cons [this elem]
    (throw (UnsupportedOperationException. "Segment.cons"))) ; maybe later
  IPersistentSet
  (contains [this elem]
    (and (integer? elem)
         (<= lo elem hi)))
  (get [this elem]
    (when (.contains this elem) elem))
  (disjoin [this elem]
    (if (not (.contains this elem))
      this
      (cond
       (= lo elem) (Segment. (+ lo 1) hi)
       (= hi elem) (Segment. lo (- hi 1))
       :else (let [mid (quot (+ lo hi) 2)
                   branch (Branch. mid (Segment. lo (- mid 1)) (Segment. mid hi))]
               (.disjoin branch elem)))))
  BoundedSet
  (lower-bound [this]
    lo)
  (upper-bound [this]
    hi)
  (gte [this bound]
    (Segment. (max lo bound) hi))
  (lte [this bound]
    (Segment. lo (min hi bound))))

(defn segment [lo hi]
  (Segment. lo hi))
