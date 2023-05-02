(ns temperament
  (:require [clojure.math :as math]
            [integer :refer [factors monzo prime-indices]]))

(defn maps-ratio?
  "Returns true if a ratio is within a temperament's prime limit."
  [t r]
  (let [period-mapping (first (t :mapping))]
    (->> (concat (factors (numerator r))
                 (factors (denominator r)))
         (every? (fn [x]
                   (let [i (prime-indices x)]
                     (and (< i (count period-mapping))
                          (some? (nth period-mapping i)))))))))

(defn map-ratio
  "Returns the mapping for ratio r in temperament t."
  [r t]
  (let [e (monzo r)]
    (->> (t :mapping)
         (map (fn [v]
                (->> v
                     (map-indexed (fn [i x]
                                    (* x (nth e i))))
                     (reduce +))))
         rest)))

(defn linear-tuning
  "Returns the cents of ratio r in a linear temperament t with the given
   mapping and generators."
  [r t]
  (if (maps-ratio? t r)
    (let [generators (t :generators)
          num-factors (factors (numerator r))
          den-factors (factors (denominator r))
          prime-count (count (first (t :mapping)))
          map-factors (fn [fs]
                        (->> (map (fn [n]
                                    (->> (t :mapping)
                                         (map #(% (prime-indices n)))
                                         (map-indexed (fn [i x]
                                                        (* (generators i) x)))
                                         (reduce +)))
                                  fs)
                             (reduce +)))]
      (if (some #(>= (prime-indices %) prime-count)
                (concat num-factors den-factors))
        nil
        (- (map-factors num-factors)
           (map-factors den-factors))))
    nil))

(defn cents-from-ratio
  "Returns the cents of an interval ratio."
  [r]
  (* 1200 (/ (math/log r) (math/log 2))))

(defn tuning-error
  "Returns the tuning error of ratio r in a regular temperament."
  [r temperament]
  (let [tuning (linear-tuning r temperament)]
    (if (nil? tuning)
      nil
      (- (cents-from-ratio r) tuning))))

(defn error-stats
  "Returns a map of error stats for consonances of interest in a given
   temperament."
  [rs t]
  (let [errors (into {} (keep (fn [r]
                                (some->> (tuning-error r t)
                                         abs
                                         (vector r)))
                              rs))
        abs-errors (map abs (vals errors))]
    {:errors errors
     :mean-error (if (not-empty abs-errors)
                   (/ (reduce + abs-errors) (count abs-errors))
                   nil)
     :max-error (if (not-empty abs-errors) (reduce max abs-errors) nil)}))
