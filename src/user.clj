(ns user
  (:require [edo]
            [interval]
            [notation]
            [number]
            [scale]
            [search]
            [summary :refer [summary]]
            [temperament]
            [var]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn save-edn
  "Write v to path as edn."
  [path v]
  (spit (format "data/%s.edn" path) (with-out-str (pprint v))))

(defn load-edn
  "Load path as edn."
  [path]
  (edn/read-string (slurp (format "data/%s.edn" path))))

(defn compute-edos-by-subgroup [m n]
  (let [size-range [m n]
        data (for [s (number/viable-subgroups)]
               {:subgroup s
                :best-edos (edo/best-in-subgroup size-range s)
                :viable-edos (map :edo (edo/in-subgroup size-range s))})]
    (save-edn (format "edos-by-subgroup-%d" var/*odd-limit*) data)))

(defn compute-subgroups-by-edo [m n]
  (save-edn (format "subgroups-by-edo-%d" var/*odd-limit*)
            (for [n (range m (inc n))]
              {:edo n
               :subgroups (-> (edo/as-temperament n number/primes)
                              temperament/errors-by-subgroup)})))

(defn score-temperament [s free-error max-error]
  (let [interval-weight (fn [r]
                          (/ (+ (/ 1 (interval/factor-sum r))
                                (/ 1 (interval/factor-sum (/ 2 r))))
                             2))
        periods-per-octave (/ 1200 (first (:generators s)))
        index-factor (fn [i]
                       (max 0 (- 1 (* i periods-per-octave 1/14))))
        genchain-score (->> (temperament/genchain 14 s 0)
                            (map :ratios)
                            (map-indexed (fn [i rs]
                                           (* (index-factor i)
                                              (reduce + (map interval-weight rs)))))
                            (reduce +))
        es (temperament/error-stats (interval/odd-limit var/*odd-limit*) s)
        error-penalty (/ (max 0 (- (:mean-error es) free-error))
                         (- max-error free-error))]
    (float (* genchain-score (- 1 error-penalty)))))

(defn print-csv-notation-matrix [n t]
  (let [gc (map :ratios (temperament/genchain (* n 2) t))
        cgc (reverse (map (fn [rs]
                            (map #(/ 2 %) rs))
                          gc))
        format-row (fn [vs]
                     (->> vs
                          (map #(str/join "~" %))
                          (str/join ", ")))]
    (->> (map format-row [(take (dec n) cgc)
                          (drop-last (drop n cgc))
                          (rest (take n gc))
                          (rest (drop n gc))])
         (str/join "\n")
         println)))

(defn switch-generator
  "Return a version of temperament t with its generator switched between
   bright and dark."
  [t]
  (-> t
      (update :generators (fn [v]
                            [(first v)
                             (- (first v) (second v))]))
      (update :mapping (fn [v]
                         [(first v) (map - (second v))]))))

(comment
  (def t (temperament/named "keen"))

  (summary t)
  (save-edn "summaries" (map summary (temperament/load-all)))

  (temperament/optimize 14 t)
  (map (fn [t] [(:name t) (temperament/optimize 14 t)])
       (temperament/load-all))

  (temperament/error-stats (temperament/flat-genchain 16 t) t)

  (switch-generator t)

  (temperament/genchain 12 t 0)
  (temperament/genchain 12 (switch-generator t) 0)

  (->> (edo/as-temperament 17 [2 3 13])
       (temperament/error-stats (interval/odd-limit var/*odd-limit*)))

  (scale/chromatic-scale (edo/as-temperament 17 [2 3]))

  (compute-edos-by-subgroup 5 72)
  (binding [var/*odd-limit* 27]
   (compute-subgroups-by-edo 5 24))

  (binding [var/*odd-limit* 9]
    (->> (load-edn "summaries")
         (map (fn [s]
                {:name (:name s)
                 :score (score-temperament s 3 20)}))
         (sort-by :score)
         reverse
         (take 10)))

  (let [n (notation/all-notation (interval/odd-limit var/*odd-limit*) t 7 5 true)]
    (for [i (range 1 9)
          q ["d" "m" "M" "A"]]
      (let [s (str q i)]
        [s (map :ratio (filter #(= (:notation %) s) n))])))
  (scale/moses [1200 (second (:generators t))] [7 13])
  (scale/moses (:generators (switch-generator t)) [10 22])

  (->> (interval/subgroup 27 [2 3 17 19])
       (map (fn [r] [r (interval/cents r)]))
       (sort-by second))

  :rcf)
