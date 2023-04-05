(ns notation
  (:require [clojure.math :as math]
            [integer :refer [factors prime-indices]]
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

(defn octave-reduce
  "Returns r, doubled or halved until within the range [1, 2]."
  [r]
  (cond (> r 2) (octave-reduce (/ r 2))
        (< r 1) (octave-reduce (* r 2))
        :else r))

(defn all-notation
  "Return all notation for intervals of interest in a scale, formatted nicely."
  [rs t n mode perfect-degrees]
  (->> (concat rs (map #(octave-reduce (/ 1 %))
                       rs))
       (filter #(temperament/maps-ratio? t %))
       (map (fn [r]
              {:ratio r
               :notation (let [note (notation r t n mode)
                               perfect (perfect-degrees (note :degree))]
                           (format-notation note perfect))}))))
