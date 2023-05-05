(ns edo
  (:require [clojure.math :as math]
            [interval]))

(defn mapping [n primes]
  (let [g (/ 1200 n)
        pset (set primes)]
    [(vec (for [p [2 3 5 7 11 13]]
            (if (pset p)
              (math/round (/ (interval/cents p) g))
              nil)))]))

(defn as-temperament [n primes]
  {:name (str n "edo")
   :mapping (mapping n primes)
   :generators [(float (/ 1200 n))]})
