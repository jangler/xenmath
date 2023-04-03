(ns xenmath
  (:require [clojure.math :as math]))

(def prime-indices
  "Map of primes to their indices in a mapping or generator list."
  {2 0
   3 1
   5 2
   7 3
   11 4})

(defn factors
  "Returns a seq of factors of n."
  [n]
  (let [i (first (filter #(zero? (rem n %)) (range 2 (inc n))))]
    (if (= i n)
      [n]
      (conj (factors (/ n i)) i))))

(defn octave-reduce
  "Returns r, doubled or halved until within the range [1, 2]."
  [r]
  (cond (> r 2) (octave-reduce (/ r 2))
        (< r 1) (octave-reduce (* r 2))
        :else r))

(defn cents-from-ratio
  "Returns the cents of an interval ratio."
  [r]
  (* 1200 (/ (math/log r) (math/log 2))))

(defn linear-tuning
  "Returns the cents of ratio r in a linear temperament with the given mapping
   and generators."
  [r temperament]
  (let [generators (temperament :generators)
        num-factors (factors (numerator r))
        den-factors (factors (denominator r))
        map-factors (fn [fs]
                      (->> (map (fn [n]
                                  (->> (temperament :mapping)
                                       (map #(% (prime-indices n)))
                                       (map-indexed (fn [i x]
                                                      (* (generators i) x)))
                                       (reduce +)))
                                fs)
                           (reduce +)))]
    (mod (- (map-factors num-factors)
            (map-factors den-factors))
         1200)))

(defn tuning-error
  "Returns the tuning error of ratio r in a regular temperament."
  [r temperament]
  (- (cents-from-ratio r)
     (linear-tuning r temperament)))

(def consonances-of-interest
  "Ratios we're interested in tuning accurately."
  [3/2 ; and 4/3
   9/8 ; and 16/9
   5/4 ; and 8/5
   5/3 ; and 6/5
   9/5 ; and 10/9
   15/8 ; and 16/15
   7/4 ; and 8/7
   7/6 ; and 12/7
   7/5 ; and 10/7
   9/7 ; and 14/9
   15/14 ; and 28/15
   ])

(defn error-stats
  "Returns a map of error stats for consonances of interest in a given
   temperament."
  [temperament]
  (let [errors (map #(abs (tuning-error % temperament))
                    consonances-of-interest)]
    {:errors errors
     :mean-error (/ (reduce + errors) (count errors))
     :max-error (reduce max errors)}))

(def septimal-meantone
  {:mapping [[1 0 -4 -13] [0 1 4 10]]
   :generators [1200 696.9521]})

(def edo12
  {:mapping [[12 7 4 10]]
   :generators [(/ 1200 12)]})

(def edo19
  {:mapping [[19 11 6 15]]
   :generators [(/ 1200 19)]})

(def edo31
  {:mapping [[31 18 10 25]]
   :generators [(/ 1200 31)]})

(def marvel
  {:mapping [[1 0 0 -5] [0 1 0 2] [0 0 1 2]]
   :generators [1200 700.4075 383.6376]})

(def edo72
  {:mapping [[72 42 23 58]]
   :generators [(/ 1200 72)]})

(def septimal-porcupine
  {:mapping [[1 2 3 2] [0 -3 -5 6]]
   :generators [1200 163.2032]})

(def edo22
  {:mapping [[22 13 7 18]]
   :generators [(/ 1200 22)]})

; meantone family
(error-stats septimal-meantone)
(error-stats edo12)
(error-stats edo19)
(error-stats edo31)

; marvel family
(error-stats marvel)
(error-stats edo72)

; porcupine family
(error-stats septimal-porcupine)
(error-stats edo22)
