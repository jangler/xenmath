(ns edo
  (:require [clojure.math :as math]
            [interval]
            [temperament]))

(defn mapping
  "Returns the best mapping of the given primes for the edo of size n."
  [n primes]
  (let [g (/ 1200 n)
        pset (set primes)]
    [(vec (for [p [2 3 5 7 11 13]]
            (if (pset p)
              (math/round (/ (interval/cents p) g))
              nil)))]))

(defn as-temperament
  "Returns n-edo as a temperament, mapped for the given primes."
  [n primes]
  {:name (str n "edo")
   :mapping (mapping n primes)
   :generators [(float (/ 1200 n))]})

(defn in-subgroup
  "Return edos from min-size to max-size that map the given primes with
   acceptable accuracy."
  [[min-size max-size] primes]
  (let [rs (interval/subgroup 15 primes)]
    (->> (range min-size (inc max-size))
         (map (fn [n]
                (let [es (->> (as-temperament n primes)
                              (temperament/error-stats rs))]
                  {:edo n
                   :mean-error (:mean-error es)
                   :max-error (:max-error es)})))
         (filter #(< (:max-error %) 15)))))

(defn strip-nondecreasing-error
  "Given a return value from in-subgroup, strip edos without lower mean and max
   error than smaller edos."
  [edos]
  (loop [remaining edos
         found []
         record-max 30
         record-mean 15]
    (if (empty? remaining)
      found
      (recur (rest remaining)
             (if (and (< (:max-error (first remaining)) record-max)
                      (< (:mean-error (first remaining)) record-mean))
               (conj found (first remaining))
               found)
             (min record-max (:max-error (first remaining)))
             (min record-mean (:mean-error (first remaining)))))))

(defn best-in-subgroup
  "Return the best edos from min-size to max-size for the given primes."
  [[min-size max-size] primes]
  (->> (in-subgroup [min-size max-size] primes)
       strip-nondecreasing-error))
