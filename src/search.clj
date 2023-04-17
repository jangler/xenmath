(ns search
  "Searches for linear temperaments matching critera."
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [scale]
            [integer :refer [monzo]]
            [temperament :refer [cents-from-ratio error-stats]]))

(def odd-limit 15)
(def smallest-consonance 10/9)
(def min-degrees-with-otonal-triads 1)
(def min-mean-otonal-triads-per-degree 1.5)
(def mos-size-range [5 9])
(def error-limit 20)
(def min-primes-mapped 3)
(def min-step-size 20)

(def consonances
  (->> (range 3 (inc odd-limit))
       (map (fn [i]
              (for [j (range (int (math/ceil (/ i 1.999))) i)]
                (/ i j))))
       (apply concat)
       (filter #(>= % smallest-consonance))
       set))

(defn edo-interval
  "Return a\\b in cents."
  [a b]
  (* a (/ 1200 b)))

(defn random-in-range
  "Return a uniformly distributed random double between a and b."
  [a b]
  (+ a (* (math/random) (- b a))))

(defn mapped-scale
  "Return a generator exponent vector representation of a MOS scale of the
   given size."
  [[per gen] n]
  (let [periods-per-octave (/ 1200 per)]
    (loop [n n
           genpos [0 0]
           scale (map #(vector (inc %) 0)
                      (range periods-per-octave))]
      (if (<= n periods-per-octave)
        (sort-by #(reduce + (map * [per gen] %))
                 scale)
        (let [y (inc (second genpos))
              x (first genpos)
              x (if (> (+ (* x per) (* y gen)) per)
                  (dec x)
                  x)]
          (recur (- n periods-per-octave)
                 [x y]
                 (concat scale (map #(vector (+ x %) y)
                                    (range periods-per-octave)))))))))

(defn mapped-next-mode
  "Returns the next mode of a mapped scale."
  [scale]
  (let [fst (first scale)
        lst (last scale)]
    (->> (concat (rest scale) [(map + fst lst)])
         (map #(map - % fst)))))

(defn mapped-modes
  "Returns all modes of a mapped scale."
  [scale]
  (loop [ms []
         scale scale
         n (count scale)]
    (if (zero? n)
      ms
      (recur (conj ms scale)
             (mapped-next-mode scale)
             (dec n)))))

(defn map-interval
  "Given a ratio and mapping, returns the exponent vector.
   If the mapping does not map the ratio, returns nil."
  [m r]
  (let [v (monzo r)]
    (if (every? (fn [[i x]]
                  (or (zero? x)
                      (and (contains? (first m) i)
                           (some? ((first m) i)))))
                (map-indexed vector v))
      (->> m
           (map (fn [xs]
                  (map #(if (nil? %) 0 %) xs)))
           (map #(reduce + (map * v %))))
      nil)))

(defn scale-consonances
  "Return consonances represented by a mode of a mapped scale."
  [mapping scale]
  (->> consonances
       (map (fn [r]
              {:ratio r
               :vector (map-interval mapping r)}))
       (filter (fn [m]
                 (and (some? (m :vector))
                      (some #(= % (m :vector)) scale))))
       (map :ratio)
       sort))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn otonal-triad-form
  "Return an otonal triad made of ratios a and b over a 1/1 root."
  [a b]
  (let [[a b] (sort [a b])
        an (numerator a)
        ad (denominator a)
        bn (numerator b)
        bd (denominator b)
        cd (lcm ad bd)]
    (map int [cd (* an (/ cd ad)) (* bn (/ cd bd))])))

(defn form-consonant-triad?
  "Return true if the ratios form a consonant triad over a 1/1 root."
  [a b]
  (->> [a b (/ (max a b) (min a b))]
       (every? #(contains? consonances %))))

(defn mode-otonal-triads
  "Given a mapping and a mode of a mapped scale, return available 15-odd-limit
   otonal triads on the scale root."
  [mapping scale]
  (let [cs (scale-consonances mapping scale)]
    (->> (combo/combinations cs 2)
         (filter #(apply form-consonant-triad? %))
         (map #(apply otonal-triad-form %)))))


(defn scale-otonal-triads
  "Return available 15-odd-limit otonal triads on each scale degree."
  [generators n mapping]
  (->> (mapped-scale generators n)
       mapped-modes
       (map #(mode-otonal-triads mapping %))))

(defn scale-matrix
  "Return a table of consonances represented by each degree of each mode."
  [gs n m]
  (let [cvs (map (fn [r]
                   {:ratio r
                    :vector (map-interval m r)})
                 consonances)]
    (->> (mapped-scale gs n)
         mapped-modes
         (map (fn [mode]
                (map (fn [v]
                       (->> cvs
                            (filter (fn [cv]
                                      (= (cv :vector) v)))
                            (map :ratio)))
                     mode))))))

(defn fraction-of-degrees-with-otonal-triads
  "Returns the fraction of mapped scale degrees with 15-odd-limit otonal
   triads."
  [generators n mapping]
  (/ (->> (scale-otonal-triads generators n mapping)
          (filter not-empty)
          count)
     n))

(defn mean-otonal-triads-per-degree
  "Returns the mean of otonal triad counts per scale degree."
  [generators n mapping]
  (float (/ (->> (scale-otonal-triads generators n mapping)
                 (map count)
                 (reduce +))
            n)))

(defn guess-vector-to-interval
  "Finds a vector that comes out within the error limit of an interval, for the
   given generators."
  [[per gen] r]
  (let [cents (cents-from-ratio r)
        period-reduced-cents (mod cents per)
        variance (* 2 (second mos-size-range))]
    (loop [ps (shuffle (range (- variance) (inc variance)))]
      (if (empty? ps)
        nil
        (let [y (first ps)]
          (if (< (abs (- period-reduced-cents (mod (* y gen) per))) error-limit)
            [(math/round (/ (- cents (* y gen)) per)) y]
            (recur (rest ps))))))))

(defn random-choice
  [coll]
  (nth coll (math/floor (random-in-range 0 (count coll)))))

(defn random-reasonable-mapping
  [[per gen]]
  (let [ms (for [prime [3 5 7 11 13]]
             (guess-vector-to-interval [per gen] prime))]
    [(vec (concat [(/ 1200 per)] (map first ms)))
     (vec (concat [0] (map second ms)))]))

(defn omit-random-primes
  [m]
  (let [avail-primes (count (first m))
        n-primes (math/round (random-in-range min-primes-mapped avail-primes))
        falses (map (fn [_] false) (range (dec n-primes)))
        trues (map (fn [_] true) (range (- avail-primes (dec n-primes))))
        omit (concat [false] (shuffle (concat trues falses)))
        omit-fn (fn [v]
                  (vec (map-indexed (fn [i x]
                                      (if (nth omit i)
                                        nil
                                        x))
                                    v)))]
    (vec (map omit-fn m))))

(defn find-viable-mapping
  "Given generators and scale size, attempts to find a mapping that satisfies
   search criteria."
  [gs n]
  (prn gs n)
  (loop [tries 1000]
    (if (zero? tries)
      nil
      (let [m (omit-random-primes (random-reasonable-mapping gs))
            errors (error-stats consonances {:generators gs :mapping m})]
        (if (and (< (errors :max-error)
                    error-limit)
                 (>= (mean-otonal-triads-per-degree gs n m)
                     min-mean-otonal-triads-per-degree)
                 (>= (fraction-of-degrees-with-otonal-triads gs n m)
                     min-degrees-with-otonal-triads))
          m
          (recur (dec tries)))))))

(defn find-proper-generator
  "Find generators that generate a proper MOS of appropriate size."
  []
  (loop []
    (let [per (/ 1200 (math/round (random-in-range 1 4)))
          gens [per (random-in-range min-step-size (/ per 2))]
          mos-sizes (->> (scale/moses gens mos-size-range)
                         (filter scale/proper?)
                         (map count))]
      (if (empty? mos-sizes)
        (recur)
        {:generators gens :mos-sizes mos-sizes}))))

(defn optimize
  ([n t] (optimize t n (fn [t]
                         (let [es (error-stats consonances t)]
                           (es :max-error)))))
  ([n t errfn]
   (loop [t t
          n n
          e (errfn t)]
     (if (pos? n)
       (let [gs (t :generators)
             t2 (assoc t :generators
                       (vec (concat [(first gs)]
                                    (->> (rest gs)
                                         (map #(+ % (- (math/random) 0.5)))))))
             e2 (errfn t2)]
         (recur (if (< e2 e) t2 t)
                (dec n)
                (if (< e2 e) e2 e)))
       t))))

(defn search
  []
  (loop [n 100]
    (if (zero? n)
      '()
      (let [x (find-proper-generator)
            finds (->> (for [n (x :mos-sizes)]
                         (let [mapping (find-viable-mapping (x :generators) n)]
                           {:mapping mapping
                            :generators (x :generators)
                            :mos-size n
                            :pattern (scale/pattern-name
                                      (first (scale/moses (x :generators) [n n])))}))
                       (filter #(some? (% :mapping)))
                       (optimize 1000))]
        (if (empty? finds)
          (recur (dec n))
          finds)))))

(comment
  (def hedgehog-gens [600 165])
  (def meantone
    {:mapping [[1 2 4] [0 -1 -4]]
     :generators [1200 503.7613]})
  (def helmholtz-mapping [[1 2 -1] [0 -1 8]])
  (error-stats consonances meantone)
  (->> (mapped-scale hedgehog-gens 8)
       (mapped-modes))
  (->> (mapped-scale [1200 504] 7))
  (->> (mapped-scale [1200 504] 7)
       mapped-modes
       (map #(mode-otonal-triads (meantone :mapping) %)))
  (->> (mapped-scale [1200 498] 7)
       mapped-modes
       (map #(mode-otonal-triads helmholtz-mapping %)))
  (def orgone
    {:mapping [[1 nil nil 2 4] [0 nil nil 3 -2]]
     :generators [1200 323.3717]})
  (error-stats consonances orgone)
  (->> (mapped-scale (orgone :generators) 7)
       mapped-modes
       (map #(mode-otonal-triads (orgone :mapping) %)))

  (find-proper-generator)
  (search)
  (def t
    {:mapping [[4 nil 9 11 17 16] [0 nil 1 1 -13 -5]]
     :generators [300 72.13966342994118]
     :mos-size 8
     :pattern "4L 4s"})
  (scale/moses (t :generators) [8 8])
  (error-stats consonances t)
  (scale-otonal-triads (t :generators) (t :mos-size) (t :mapping))
  (scale-matrix (t :generators) (t :mos-size) (t :mapping))

  :rcf)
