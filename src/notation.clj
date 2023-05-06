(ns notation
  (:require [clojure.math :as math]
            [number :refer [factors prime-indices rational-power]]
            [interval :refer [monzo]]
            [temperament]))

(defn mod-within
  "Like modulo, but returns a value between a and b instead of within 0 and n."
  [x a b]
  (let [n (inc (- b a))]
    (loop [x x]
      (cond (< x a) (recur (+ x n))
            (> x b) (recur (- x n))
            :else x))))

(defn index-of
  "Returns the index of x in xs"
  [x xs]
  (->> (keep-indexed (fn [i y]
                       (if (= y x)
                         i
                         nil))
                     xs)
       first))

(defn rotate
  "Returns a coll rotated left by n items."
  [n coll]
  (concat (drop n coll) (take n coll)))

(defn genchain-index-scale
  "Returns a sequence of genchain indices in scale order, given a linear
   temperament, scale size, and mode."
  [t n mode]
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

(defn notation
  "Returns the name for an interval, given a linear temperament, scale size,
   and mode."
  [r t n mode reverse-chroma]
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
     :sharps (if reverse-chroma
               (- sharps)
               sharps)}))

(defn format-notation
  "Returns a string version of a map with keys :degree and :sharps."
  [m perfect]
  (if (and (<= (m :sharps) 1)
           (>= (m :sharps) (if perfect -1 -2)))
    (str (if perfect
           (case (m :sharps)
             -1 "d"
             0 "P"
             1 "A")
           (case (m :sharps)
             -2 "d"
             -1 "m"
             0 "M"
             1 "A"))
         (m :degree))
    nil))

(defn octave-reduce
  "Returns r, doubled or halved until within the range [1, 2]."
  [r]
  (cond (> r 2) (octave-reduce (/ r 2))
        (< r 1) (octave-reduce (* r 2))
        :else r))

(defn all-notation
  "Return all notation for intervals of interest in a scale, formatted nicely."
  ([rs t n mode reverse-chroma] (all-notation rs t n mode reverse-chroma #{1}))
  ([rs t n mode reverse-chroma perfect-intervals]
   (->> rs
        (filter #(temperament/maps? t %))
        (map (fn [r]
               {:ratio r
                :notation (let [note (notation r t n mode reverse-chroma)
                                perfect (perfect-intervals (note :degree))]
                            (format-notation note perfect))}))
        (filter (comp some? :notation)))))

(defn notate-planar
  "Notate a ratio in a planar temperament with a fifth generator."
  [r t]
  (let [m (temperament/tmap r t)
        degree (nth [1 5 2 6 3 7 4] (mod (first m) 7))
        chroma (math/floor-div (inc (first m)) 7)
        quality (if (#{1 4 5} degree)
                  (case chroma
                    -3 "ddd"
                    -2 "dd"
                    -1 "d"
                    0 "P"
                    1 "A"
                    2 "AA")
                  (case chroma
                    -3 "dd"
                    -2 "d"
                    -1 "m"
                    0 "M"
                    1 "A"
                    2 "AA"))
        downs (apply str (map (fn [_] "v") (range (- (second m)))))
        ups (apply str (map (fn [_] "^") (range (second m))))
        drops (if (> (count m) 2)
                (apply str (map (fn [_] "\\") (range (- (nth m 2)))))
                "")
        lifts (if (> (count m) 2)
                (apply str (map (fn [_] "/") (range (nth m 2))))
                "")]
    (str drops lifts downs ups quality degree)))

(defn udn-degree [tertial-r]
  (nth [4 1 5 2 6 3 7] (mod (inc (second (monzo tertial-r))) 7)))

(defn udn-quality [tertial-r degree]
  (let [distance (math/floor-div (inc (second (monzo tertial-r))) 7)]
    (if (#{1 4 5} degree)
      (case distance
        -1 "d"
        0 "P"
        1 "A")
      (case distance
        -1 "m"
        0 "M"
        1 "A"))))

(defn repeat-str [s n]
  (apply str (repeat n s)))

(defn udn [r]
  (let [m (monzo r)
        tertial-r (* r (reduce * (map rational-power [1 1 81/80 64/63 32/33] m)))
        d (udn-degree tertial-r)
        q (udn-quality tertial-r d)]
    (str (repeat-str "v" (max 0 (nth m 2)))
         (repeat-str "^" (max 0 (- (nth m 2))))
         (repeat-str "\\" (max 0 (nth m 3)))
         (repeat-str "/" (max 0 (- (nth m 3))))
         (repeat-str "+" (max 0 (nth m 4)))
         (repeat-str "-" (max 0 (- (nth m 4))))
         q d)))
