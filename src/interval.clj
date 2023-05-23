(ns interval
  (:require [clojure.math :as math]
            [clojure.string :as str]
            [number]
            [clojure.math.combinatorics :as combo]))

(defn monzo
  "Returns a prime exponent vector for ratio r."
  [r]
  (let [num-factors (number/factors (if (ratio? r)
                                      (numerator r)
                                      r))
        den-factors (number/factors (if (ratio? r)
                                      (denominator r)
                                      1))]
    (vec (for [p number/primes]
           (- (count (filter #(= % p) num-factors))
              (count (filter #(= % p) den-factors)))))))

(defn factor-sum
  "Returns the sum of prime factors for ratio r."
  [r]
  (->> (monzo r)
       (map abs)
       (map * number/primes)
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

(defn beating-dissonance
  "Return a score for beating dissonance in interval r, from 0 to 1."
  [r]
  (let [c (cents r)]
    (cond
      (< c 60) (/ c 60)
      (< c 120) (- 1 (/ (- c 60) 120))
      (< c 180) (- 1/2 (/ (- c 120) 180))
      (< c 240) (- 1/6 (/ (- c 180) 360))
      :else 0)))

(defn tension
  "Return an estimate of harmonic clashing in interval r."
  [r]
  (let [hs1 (for [i (range 1 17)]
              {:pitch i
               :volume (/ i)})
        hs2 (for [h hs1]
              (update h :pitch #(* r %)))]
    (reduce + (for [h1 hs1
                    h2 hs2]
                (let [[p1 p2] (map :pitch [h1 h2])]
                  (* (beating-dissonance (/ (max p1 p2) (min p1 p2)))
                     (:volume h1)
                     (:volume h2)))))))

(defn from-cents
  "Convert cents to a (float) ratio."
  [c]
  (math/pow 2 (/ c 1200)))

; this doesn't really give reasonable results
(defn chord-tension
  "Return an estimate of harmonic clashing in chord rs."
  [rs]
  (let [hs (for [r rs
                 i (range 1 17)]
             {:pitch (* r i)
              :volume (/ i)})]
    (->> (combo/combinations hs 2)
         (map (fn [[h1 h2]]
                (let [[p1 p2] (map :pitch [h1 h2])]
                  (* (beating-dissonance (/ (max p1 p2) (min p1 p2)))
                     (:volume h1)
                     (:volume h2)))))
         (reduce +))))

(comment
  (->> (for [c (range 2400)]
         [c (tension (from-cents c))])
       flatten
       vec
       (str/join " ")
       (spit "xy.txt"))
  
  (->> (interval/odd-limit 15)
       (sort-by tension))
  
  (interval/cents 15/11)
  
  (chord-tension [1/1 5/4 3/2 7/4])
  :rcf)
