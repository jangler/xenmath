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
        data (for [t (number/viable-subgroups)]
               {:subgroup t
                :best-edos (edo/best-in-subgroup size-range t)
                :viable-edos (map :edo (edo/in-subgroup size-range t))})]
    (save-edn (format "edos-by-subgroup-%d" var/*odd-limit*) data)))

(defn compute-subgroups-by-edo [ns]
  (let [filename (format "subgroups-by-edo-%d" var/*odd-limit*)]
    (loop [ns ns
           results []]
      (when (seq ns)
        (let [n (first ns)
              m {:edo n
                 :subgroups (-> (edo/as-temperament n number/primes)
                                temperament/errors-by-subgroup)}
              results (conj results m)]
          (save-edn filename results)
          (recur (rest ns) results))))))

(defn score-temperament [t free-error max-error]
  (let [interval-weight (fn [r]
                          (/ (+ (/ 1 (interval/factor-sum r))
                                (/ 1 (interval/factor-sum (/ 2 r))))
                             2))
        periods-per-octave (/ 1200 (first (:generators t)))
        index-factor (fn [i]
                       (max 0 (- 1 (* i periods-per-octave 1/14))))
        genchain-score (->> (temperament/genchain 14 t 0)
                            (map :ratios)
                            (map-indexed (fn [i rs]
                                           (* (index-factor i)
                                              (reduce + (map interval-weight rs)))))
                            (reduce +))
        es (temperament/error-stats (interval/odd-limit var/*odd-limit*) t)
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
  (def t (temperament/named "catakleismic"))

  (def t {:name "superpyth",
          :mapping [[1 2 6 2 4] [0 -1 -9 2 1] [0 0 0 0 1]],
          :generators [1200 490.4096 89]})
  (notation/notate-planar 3/2 t)

  (summary t)
  (binding [var/*error-tolerance* 20]
    (save-edn "summaries" (map summary (temperament/load-all))))

  (edo/supporting (range 5 54) t)

  (binding [var/*odd-limit* 15]
    (temperament/optimize 18 t))

  (temperament/error-stats (temperament/flat-genchain 16 t) t)

  (switch-generator t)

  (temperament/genchain 12 t 0)
  (temperament/genchain 12 (switch-generator t) 0)

  (->> (edo/as-temperament 17 [2 3 13])
       (temperament/error-stats (interval/odd-limit var/*odd-limit*)))

  (scale/chromatic-scale (edo/as-temperament 17 [2 3]))

  (binding [var/*odd-limit* 9]
    (->> [[2 3 5] [2 3 7] [2 3 5 7]]
         (map #(interval/subgroup 9 %))
         (map #(select-keys (->> (edo/as-temperament 53 [2 3 5 7])
                                 (temperament/error-stats %))
                            [:mean-error :max-error]))))

  (future (binding [var/*odd-limit* 9]
            (compute-edos-by-subgroup 5 99)
            (compute-subgroups-by-edo (range 5 (inc 99)))))

  (binding [var/*odd-limit* 15]
    (->> (load-edn "summaries")
         (map (fn [t]
                {:name (:name t)
                 :score (score-temperament t 3 15)}))
         (sort-by :score)
         reverse
         (take 10)))

  (scale/brightest-mode-index (scale/moses (:generators t) [6 6]))
  (let [n (notation/all-notation (interval/odd-limit 15) t 7 5 true #{1})]
    (->> (for [i (range 1 8)
               q ["dd" "d" "m" "P" "M" "A" "AA"]]
           (let [t (str q i)]
             [t (map :ratio (filter #(= (:notation %) t) n))]))
         (filter (comp not-empty second))))
  (scale/moses [1200 (second (:generators t))] [7 13])
  (->> (scale/moses (:generators t) [7 12])
       first
       scale/modes)

  (->> (interval/subgroup 27 [2 3 17 19])
       (map (fn [r] [r (interval/cents r)]))
       (sort-by second))

  (let [t (edo/as-temperament 20 [2 7 11 13 19 29])]
    (map #(temperament/tmap t %) [22/19 29/19]))

  (let [t (edo/as-temperament 24 [2 3 11 17 19])
        es (temperament/error-stats (interval/odd-limit 31) t)]
    [(:mean-error es)
     (:max-error es)
     (count (:errors es))
     (->> es
          :errors
          (map #(temperament/tmap t (first %)))
          set
          count)])
  (map interval/cents [13/7 13/8 13/9 13/10 13/11 13/12])

  (search/scale-matrix (assoc t :mos-size 7))

  (->> (interval/odd-limit 15)
       (map (fn [r]
              {:ratio r
               :cents (interval/cents r)}))
       (sort-by :cents))

  ; TODO make this work right with rationals
  (scale/modes [9/8 5/4 11/8 3/2 5/3 7/4 2/1])

  :rcf)
