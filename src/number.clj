(ns number 
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]))

(defn factors
  "Returns a seq of factors of n."
  [n]
  (if (= n 1)
    []
    (let [i (first (filter #(zero? (rem n %)) (range 2 (inc n))))]
      (if (= i n)
        [n]
        (conj (factors (/ n i)) i)))))

(def prime-indices
  "Map of primes to their indices in a mapping or generator list."
  {2 0
   3 1
   5 2
   7 3
   11 4
   13 5})

(defn rational-power [r p]
  (cond (zero? p) 1
        (pos? p) (* r (rational-power r (dec p)))
        :else (/ (rational-power r (inc p)) r)))

(defn places
  "Returns x rounded to n decimal places."
  [n x]
  (let [f (math/pow 10 n)]
    (/ (math/round (* f x)) f)))

(def viable-subgroups
  "15-odd-limit subgroups that map 2, and at least one of 3 and 5."
  (let [primes [2 3 5 7 11 13]]
    (for [n (range 3 (inc (count primes)))
          c (combo/combinations [2 3 5 7 11 13] n)
          :when (and (some #{2} c)
                     (or (some #{3} c)
                         (some #{5} c)))]
      c)))
