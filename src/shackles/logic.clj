(ns shackles.logic
  (:require [shackles.core :as core]
            [mist.strict :as strict]))

(defrecord Assign [value])

(defrecord LogicDomain [value] ; either [value] or nil
  Domain
  (size [dom]
    (if (nil? dom)
      nil
      1))
  (values ))
