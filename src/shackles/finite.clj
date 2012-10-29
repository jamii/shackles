(ns shackles.finite
  (:require [shackles.kernel :as kernel]
            [shackles.search :as search])
  (:import [shackles.kernel Equal NotEqual]
           [shackles.search Space]))

(defrecord GTE [value]) ; greater-than-or-equal-to
(defrecord LTE [value]) ; less-than-or-equal-to

(defprotocol LowerBoundedDomain ; must handle GTE action
  (lower-bound [dom] "Returns the (inclusive) lower bound of this domain"))

(defprotocol UpperBoundedDomain ; must handle LTE action
  (upper-bound [dom] "Returns the (inclusive) upper bound of this domain"))

(defprotocol FiniteDomain
  (num-elems [dom] "Returns the number of values in this domain.")
  (elems [dom] "Returns a lazy seq of values in this domain."))

(defprotocol Chooser
  (choose [chooser vars&doms] "Chooses a [var dom] pair from vars&doms, or return nil if non are suitable."))

(defrecord ChooseUnassigned []
  Chooser
  (choose [_ vars&doms]
    (first (filter (fn [[_ dom]] (not (kernel/assigned? dom))) vars&doms))))

(defprotocol Splitter
  (split [splitter dom] "Returns a seq of actions which create subdoms whose union is the original dom"))

(defrecord SplitAssign []
  Splitter
  (split [_ dom]
    (when-not (kernel/assigned? dom)
      (let [value (first (elems dom))]
        (list (Equal. value) (NotEqual. value))))))

(defrecord Exhaust [vars chooser splitter]
  Space
  (children [this root]
    (when-let [[var dom] (choose chooser (kernel/select-vars root vars))]
      (let [actions (split splitter dom)
            children (map #(kernel/handle-action root var %) actions)]
        (for [child children] [this child])))))
