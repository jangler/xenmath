(ns integer)

(defn factors
  "Returns a seq of factors of n."
  [n]
  (let [i (first (filter #(zero? (rem n %)) (range 2 (inc n))))]
    (if (= i n)
      [n]
      (conj (factors (/ n i)) i))))

(def prime-indices
  "Map of primes to their indices in a mapping or generator list."
  {2 0
   3 1
   5 2
   7 3
   11 4
   13 5})
