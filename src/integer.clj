(ns integer)

(defn factors
  "Returns a seq of factors of n."
  [n]
  (let [i (first (filter #(zero? (rem n %)) (range 2 (inc n))))]
    (if (= i n)
      [n]
      (conj (factors (/ n i)) i))))

(defn monzo
  "Returns a prime exponent vector for ratio r."
  [r]
  (let [num-factors (factors (numerator r))
        den-factors (factors (denominator r))]
    (vec (for [p [2 3 5 7 11]]
           (- (count (filter #(= % p) num-factors))
              (count (filter #(= % p) den-factors)))))))

(def prime-indices
  "Map of primes to their indices in a mapping or generator list."
  {2 0
   3 1
   5 2
   7 3
   11 4
   13 5})
