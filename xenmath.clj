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
        prime-count (count (first (temperament :mapping)))
        map-factors (fn [fs]
                      (->> (map (fn [n]
                                  (->> (temperament :mapping)
                                       (map #(% (prime-indices n)))
                                       (map-indexed (fn [i x]
                                                      (* (generators i) x)))
                                       (reduce +)))
                                fs)
                           (reduce +)))]
    (if (some #(>= (prime-indices %) prime-count)
              (concat num-factors den-factors))
      nil
      (mod (- (map-factors num-factors)
              (map-factors den-factors))
           1200))))

(defn tuning-error
  "Returns the tuning error of ratio r in a regular temperament."
  [r temperament]
  (let [tuning (linear-tuning r temperament)]
    (if (nil? tuning)
      nil
      (- (cents-from-ratio r) tuning))))

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
   11/6 ; and 12/11
   11/7 ; and 14/11
   11/8 ; and 16/11
   11/9 ; and 18/11
   11/10 ; and 20/11
   15/11 ; and 22/15
   ])

(defn error-stats
  "Returns a map of error stats for consonances of interest in a given
   temperament."
  [temperament]
  (let [errors (map abs (keep #(tuning-error % temperament)
                              consonances-of-interest))]
    {:errors errors
     :mean-error (/ (reduce + errors) (count errors))
     :max-error (reduce max errors)}))

(defn step-sizes
  "Returns a set of step sizes in a scale."
  [scale]
  (loop [sizes #{}
         remaining scale]
    (if (< (count remaining) 2)
      (= (count sizes) 2)
      (recur (let [size (- (first remaining) (second remaining))]
               (if (not-any? #(< (abs (- size %)) 1) sizes)
                 (conj sizes size)
                 sizes))
             (rest remaining)))))

(defn mos?
  "Returns true if a scale is a MOS scale."
  [scale]
  (= (count (step-sizes scale)) 2))

; TODO: it would be nice if this gave the nicest mode
(defn viable-mos
  "Returns a linear temperament's closest MOS to 7 notes."
  [temperament]
  (loop [notes [0]
         candidate nil]
    (if (<= (count notes) 12)
      (recur (conj notes (mod (+ (second (temperament :generators))
                                 (last notes))
                              1200))
             (if (and (mos? (conj (vec (sort notes)) 1200))
                      (or (nil? candidate)
                          (<= (abs (- (count notes) 7))
                              (abs (- (count candidate) 7)))))
               notes
               candidate))
      (conj (vec (sort candidate)) 1200))))

(defn chroma
  "Returns the chroma of a MOS scale; that is, the difference between its
   large and small steps."
  [scale]
  (->> (step-sizes scale)
       sort
       reverse
       (reduce -)))

(defn rotate
  "Returns a coll rotated left by n items."
  [n coll]
  (concat (drop n coll) (take n coll)))

(defn genchain-index-scale
  "Returns a sequence of genchain indices in scale order, given a linear
   temperament, scale size, and mode."
  [t n mode]
  ; TODO: implement mode argument
  (loop [notes [{:cents 0 :index 0}]]
    (if (= (count notes) n)
      (let [scale (->> notes
                       (sort-by :cents)
                       (map :index)
                       (rotate mode))
            base (first scale)]
        (map #(- % base) scale))
      (let [last-note (apply (partial max-key :index) notes)]
        (recur (conj notes {:cents (mod (+ (last-note :cents)
                                           (second (t :generators)))
                                        1200)
                            :index (inc (last-note :index))}))))))

(defn index-of
  "Returns the index of x in xs"
  [x xs]
  (->> (keep-indexed (fn [i y]
                       (if (= y x)
                         i
                         nil))
                     xs)
       first))

(defn mod-within
  "Like modulo, but returns a value between a and b instead of within 0 and n."
  [x a b]
  (let [n (inc (- b a))]
    (loop [x x]
      (cond (< x a) (recur (+ x n))
            (> x b) (recur (- x n))
            :else x))))

(defn notation
  "Returns the name for an interval, given a linear temperament, scale size,
   and mode."
  [r t n mode]
  (let [num-factors (factors (numerator r))
        den-factors (factors (denominator r))
        reduce-factors (fn [fs]
                         (->> fs
                              (map (fn [f]
                                     ((second (t :mapping)) (prime-indices f))))
                              (reduce +)))
        distance (- (reduce-factors num-factors)
                    (reduce-factors den-factors))
        scale (genchain-index-scale t n mode)
        scale-min (reduce min scale)
        scale-max (reduce max scale)
        degree (inc (index-of (mod-within distance scale-min scale-max) scale))
        sharps (math/floor-div (- distance scale-min) n)]
    {:degree degree
     :sharps sharps}))

(defn format-notation
  "Returns a string version of a map with keys :degree and :sharps."
  ([m] (format-notation m false))
  ([m perfect]
   (str (if perfect
          (case (m :sharps)
            -2 "dd"
            -1 "d"
            0 "P"
            1 "A"
            2 "AA")
          (case (m :sharps)
            -2 "d"
            -1 "m"
            0 "M"
            1 "A"
            2 "AA"))
        (m :degree))))

(defn ratio-in-tuning?
  "Returns true if a ratio is within a temperament's prime limit."
  [r t]
  (let [prime-count (count (first (t :mapping)))]
    (->> (concat (factors (numerator r))
                 (factors (denominator r)))
         (every? #(< (prime-indices %) prime-count)))))

(defn all-notation
  "Return all notation for intervals of interest in a scale, formatted nicely."
  [t n mode perfect-degrees]
  (->> (concat consonances-of-interest
               (map #(octave-reduce (/ 1 %))
                    consonances-of-interest))
       (filter #(ratio-in-tuning? % t))
       (map (fn [r]
              {:ratio r
               :notation (let [note (notation r t n mode)
                               perfect (perfect-degrees (note :degree))]
                           (format-notation note perfect))}))))

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

(def srutal
  {:mapping [[2 0 11 -42] [0 1 -2 15]]
   :generators [600 704.814]})

(def pajara
  {:mapping [[2 2 7 8] [0 1 -2 -2]]
   :generators [600 706.843]})

(def superpyth
  {:mapping [[1 0 -12 6] [0 1 9 -2]]
   :generators [1200 710.291]})

(def magic
  {:mapping [[1 9 2 -1] [0 5 1 12]]
   :generators [1200 380.352]})

(def orwell
  {:mapping [[1 0 3 1 3] [0 7 -3 8 2]]
   :generators [1200 271.426]})

(def edo53
  {:mapping [[53 31 17 43 24]]
   :generators [(/ 1200 53)]})

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

; diaschismic family
(error-stats srutal)
(error-stats pajara)

; superpyth
(error-stats superpyth)

; magic
(error-stats magic)

; semicomma family
(error-stats orwell)
(error-stats edo53)

; 2 seems like the best mode for Orwell. Like mode 4 of Meantone, it has the
; generator and its inverse, and all non-perfect intervals are large and can
; therefore rightly be called major -- so this is the "major scale" of Orwell.
(def orwell9 (viable-mos orwell))
(chroma orwell9)
(all-notation orwell 9 2 #{1 3 8})

(def meantone7 (viable-mos septimal-meantone))
(chroma meantone7)
(all-notation septimal-meantone 7 4 #{1 4 5})
