(ns search
  "Searches for linear temperaments matching critera."
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as combo]
            [scale]
            [integer :refer [monzo prime-indices]]
            [temperament :refer [cents-from-ratio error-stats]]))

; TODO: chromaticism
; TODO: mapping non-primes (9 and 15)

(def odd-limit 15)
(def smallest-consonance 10/9)
(def min-degrees-with-otonal-triads 1)
(def min-mean-otonal-triads-per-degree 3)
(def mos-size-range [5 9])
(def error-limit 20)
(def min-primes-mapped 3)
(def min-step-size 30)
(def min-consonance-fraction 2/3)

(def consonances
  (->> (range 3 (inc odd-limit))
       (map (fn [i]
              (for [j (range (int (math/ceil (/ i 1.999))) i)]
                (/ i j))))
       (apply concat)
       (filter #(>= % smallest-consonance))
       set))

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
  [t]
  (->> (mapped-scale (t :generators) (t :mos-size))
       mapped-modes
       (map #(mode-otonal-triads (t :mapping) %))))

(defn scale-matrix
  "Return a table of consonances represented by each degree of each mode."
  [t]
  (let [cvs (map (fn [r]
                   {:ratio r
                    :vector (map-interval (t :mapping) r)})
                 consonances)]
    (->> (mapped-scale (t :generators) (t :mos-size))
         mapped-modes
         (map (fn [mode]
                (map (fn [v]
                       (->> cvs
                            (filter (fn [cv]
                                      (= (cv :vector) v)))
                            (map :ratio)))
                     (take (dec (t :mos-size)) mode)))))))

(defn fraction-of-degrees-with-otonal-triads
  "Returns the fraction of mapped scale degrees with 15-odd-limit otonal
   triads."
  [t]
  (/ (->> (scale-otonal-triads t)
          (filter not-empty)
          count)
     (t :mos-size)))

(defn mean-otonal-triads-per-degree
  "Returns the mean of otonal triad counts per scale degree."
  [t]
  (float (/ (->> (scale-otonal-triads t)
                 (map count)
                 (reduce +))
            (t :mos-size))))

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

(defn fraction-of-consonant-intervals
  [t]
  (/ (->> (scale-matrix t)
          (apply concat)
          (filter not-empty)
          count)
     (* (t :mos-size) (dec (t :mos-size)))))

(defn meets-criteria? [t]
  (and (>= (mean-otonal-triads-per-degree t)
           min-mean-otonal-triads-per-degree)
       (>= (fraction-of-degrees-with-otonal-triads t)
           min-degrees-with-otonal-triads)
       (>= (fraction-of-consonant-intervals t)
           min-consonance-fraction)
       (< ((error-stats consonances t) :max-error)
          error-limit)))

(defn find-viable-mapping
  "Given generators and scale size, attempts to find a mapping that satisfies
   search criteria."
  [gs n]
  (loop [tries 1000]
    (if (zero? tries)
      nil
      (let [t {:mapping (omit-random-primes (random-reasonable-mapping gs))
               :generators gs
               :mos-size n}]
        (if (meets-criteria? t)
          (t :mapping)
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
  ([n t] (optimize n t (fn [t]
                         ((error-stats consonances t) :max-error))))
  ([n t errfn]
   (prn t)
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

(defn add-additional-mappings
  "Attempt to add more prime mappings to a temperament, as long as they don't
   increase maximum error."
  [t]
  (loop [primes [3 5 7 11 13]
         t t]
    (if (empty? primes)
      t
      (let [p (first primes)
            i (prime-indices p)
            v (guess-vector-to-interval (t :generators) p)
            m (t :mapping)]
        (recur (rest primes)
               (if (and (some? v)
                        (nil? (nth (first m) i)))
                 (let [t2 (assoc t :mapping
                                 [(assoc (first m) i (first v))
                                  (assoc (second m) i (second v))])
                       e (error-stats consonances t)
                       e2 (error-stats consonances t2)]
                   (if (<= (e2 :max-error) (e :max-error))
                     t2
                     t))
                 t))))))

(defn strip-unused-mappings
  "Remove mappings from a temperament that don't make a difference in the
   scale intervals."
  [t]
  (loop [primes [3 5 7 11 13]
         t t]
    (if (empty? primes)
      t
      (let [p (first primes)
            i (prime-indices p)
            m (t :mapping)
            t2 (assoc t :mapping
                      [(assoc (first m) i nil)
                       (assoc (second m) i nil)])]
        (recur (rest primes)
               (if (= (mean-otonal-triads-per-degree t2)
                      (mean-otonal-triads-per-degree t))
                 t2
                 t))))))
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
                       (map #(->> %
                                  (optimize 1000)
                                  add-additional-mappings
                                  strip-unused-mappings
                                  (optimize 1000))))]
        (if (empty? finds)
          (recur (dec n))
          finds)))))

(comment
  (def hedgehog-gens [600 165])
  (def meantone
    {:mapping [[1 2 4] [0 -1 -4]]
     :generators [1200 503.7613]
     :mos-size 7})
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
     :generators [1200 323.3717]
     :mos-size 7})
  (error-stats consonances orgone)
  (->> (mapped-scale (orgone :generators) 7)
       mapped-modes
       (map #(mode-otonal-triads (orgone :mapping) %)))

  (search)

  (def oodako-ish
    {:mapping [[3 3 3 4 6 nil] [0 4 9 10 10 nil]]
     :generators [400 175.90928084154478]
     :mos-size 9
     :pattern "6L 3s"})
  (scale-matrix oodako-ish)
  (scale-otonal-triads oodako-ish)
  (error-stats consonances oodako-ish)

  (def dimipent
    {:mapping [[4 6 9 nil nil nil] [0 1 1 nil nil nil]],
     :generators [300 101.95509466107227],
     :mos-size 8,
     :pattern "4L 4s"})
  (scale-matrix dimipent)
  (scale-otonal-triads dimipent)
  (error-stats consonances dimipent)

  (def t
    {:mapping [[2 nil 5 6 nil nil] [0 nil -1 -1 nil nil]],
     :generators [600 213.73146960668458],
     :mos-size 6,
     :pattern "4L 2s"})
  (scale-matrix t)
  (scale-otonal-triads t)

  (def orwell
    {:mapping [[1 0 3 1 3 nil] [0 7 -3 8 2 nil]]
     :generators [1200 271.1032645156822]
     :mos-size 9
     :pattern "4L 5s"})
  (scale-matrix orwell)
  (scale-otonal-triads orwell)
  (error-stats consonances orwell)

  (def sensi
    {:mapping [[1 -1 -1 -2 nil nil] [0 7 9 13 nil nil]],
     :generators [1200 443.52209743349977],
     :mos-size 8,
     :pattern "3L 5s"})
  (scale-matrix sensi)
  (scale-otonal-triads sensi)
  (error-stats consonances sensi)

  (def meantone
    {:mapping [[1 2 4 nil nil nil] [0 -1 -4 nil nil nil]],
     :generators [1200 503.4213338965872],
     :mos-size 7,
     :pattern "5L 2s"})
  (scale-matrix meantone)
  (scale-otonal-triads meantone)
  (error-stats consonances meantone)

  ; can't find a named augmented family temperament with a ~10/9 generator
  (def t
    {:mapping [[3 7 11 12 14 nil] [0 -5 -9 -8 -8 nil]],
     :generators [400 179.3376238921682],
     :mos-size 9,
     :pattern "6L 3s"})
  (scale-matrix t)
  (scale-otonal-triads t)
  (error-stats consonances t)

  (def crepuscular
    {:mapping [[2 2 3 4 6 nil] [0 5 7 7 4 nil]]
     :generators [600 140.73473230631686]
     :mos-size 8
     :pattern "2L 6s"})
  (scale-matrix crepuscular)
  (scale-otonal-triads crepuscular)
  (error-stats consonances crepuscular)

  (def doublewide
    {:mapping [[2 5 6 7 nil nil] [0 -4 -3 -3 nil nil]],
     :generators [600 274.97119474976114],
     :mos-size 6,
     :pattern "4L 2s"})
  (scale-matrix doublewide)
  (scale-otonal-triads doublewide)
  (error-stats consonances doublewide)

  (def t
    {:mapping [[2 nil 4 5 6 nil] [0 nil 2 2 3 nil]],
     :generators [600 188.21718641121893],
     :mos-size 8,
     :pattern "6L 2s"})
  (scale-matrix t)
  (scale-otonal-triads t)
  :rcf)
