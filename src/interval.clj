(ns interval
  (:require [clojure.math :as math]
            [number]))

(defn monzo
  "Returns a prime exponent vector for ratio r."
  [r]
  (let [num-factors (number/factors (if (ratio? r)
                                       (numerator r)
                                       r))
        den-factors (number/factors (if (ratio? r)
                                       (denominator r)
                                       1))]
    (vec (for [p [2 3 5 7 11 13]]
           (- (count (filter #(= % p) num-factors))
              (count (filter #(= % p) den-factors)))))))

(defn factor-sum
  "Returns the sum of prime factors for ratio r."
  [r]
  (->> (monzo r)
       (map abs)
       (map * [2 3 5 7 11 13])
       (reduce +)))

(defn odd-limit
  "Returns ratios within an odd limit."
  [n]
  (->> (range 3 (inc n))
       (map (fn [i]
              (for [j (range (int (math/ceil (/ i 1.999))) i)]
                [(/ i j) (* 2 (/ j i))])))
       flatten
       (sort-by #(vector [(factor-sum %) %]))
       dedupe))

(defn cents
  "Returns the cents of interval r."
  [r]
  (* 1200 (/ (math/log r) (math/log 2))))

(defn primes
  "Returns primes in interval r."
  [r]
  (mapcat number/factors [(numerator r) (denominator r)]))

(defn limit
  "Returns the highest prime in interval r."
  [r]
  (reduce max (primes r)))

(defn subgroup
  "Return ratios within odd limit n, containing only primes in ps."
  [n ps]
  (let [ps (set ps)]
    (filter #(every? ps (interval/primes %))
            (odd-limit n))))
