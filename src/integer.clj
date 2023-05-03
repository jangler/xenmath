(ns integer)

(defn factors
  "Returns a seq of factors of n."
  [n]
  (if (= n 1)
    []
    (let [i (first (filter #(zero? (rem n %)) (range 2 (inc n))))]
      (if (= i n)
        [n]
        (conj (factors (/ n i)) i)))))

(defn monzo
  "Returns a prime exponent vector for ratio r."
  [r]
  (let [num-factors (factors (if (ratio? r)
                               (numerator r)
                               r))
        den-factors (factors (if (ratio? r)
                               (denominator r)
                               1))]
    (vec (for [p [2 3 5 7 11 13]]
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

(defn rational-power [r p]
  (cond (zero? p) 1
        (pos? p) (* r (rational-power r (dec p)))
        :else (/ (rational-power r (inc p)) r)))
