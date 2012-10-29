(ns shackles.int
  (:require [shackles.kernel :as kernel]
            [shackles.data.intset :as intset]
            [mist.strict :as strict])
  (:import [shackles.kernel Equal NotEqual Domain]
           [shackles.finite FiniteDomain LTE GTE LowerBoundedDomain UpperBoundedDomain]))

(defrecord IntDomain [ints]
  Domain
  (assigned? [dom]
    (= (intset/lower-bound ints) (intset/upper-bound ints)))
  (assigned [dom]
    (when (kernel/assigned? dom) (first (seq ints))))
  (perform [dom action]
    (strict/match-type action
      [Equal {:value value}] (IntDomain.
                              (if (contains? ints value)
                                (intset/segment value value)
                                (empty ints)))
      [NotEqual {:value value}] (IntDomain. (disj ints value))
      [LTE {:value value}] (IntDomain. (intset/lte ints value))
      [GTE {:value value}] (IntDomain. (intset/gte ints value))))
  (status [old-dom new-dom action]
    (if (empty? (:ints new-dom)) :failed :ok))
  (events [old-dom new-dom action]
    (let [old-lo (intset/lower-bound (.ints old-dom))
          old-hi (intset/upper-bound (.ints old-dom))
          new-lo (intset/lower-bound (.ints new-dom))
          new-hi (intset/upper-bound (.ints new-dom))]
      (concat
       (when (and (= new-lo new-hi) (not (= old-lo old-hi))) [(Equal. new-lo)])
       (when (not (= old-lo new-lo)) [(GTE. new-lo)])
       (when (not (= old-hi new-hi)) [(LTE. new-hi)]))))
  FiniteDomain
  (num-elems [dom]
    (count ints))
  (elems [dom]
    (seq ints))
  LowerBoundedDomain
  (lower-bound [dom]
    (intset/lower-bound ints))
  UpperBoundedDomain
  (upper-bound [dom]
    (intset/upper-bound ints)))

(defn int [lo hi]
  (IntDomain. (intset/segment lo hi)))
