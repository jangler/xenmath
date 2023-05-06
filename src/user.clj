(ns user
  (:require [edo]
            [interval]
            [number]
            [scale]
            [search]
            [summary :refer [summary]]
            [temperament]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]))

(defn save-edn
  "Write v to path as edn."
  [path v]
  (spit (format "data/%s.edn" path) (with-out-str (pprint v))))

(defn load-edn
  "Load path as edn."
  [path]
  (edn/read-string (slurp (format "data/%s.edn" path))))

(defn compute-edos-by-subgroup []
  (let [size-range [15 99]
        data (for [s number/viable-subgroups]
               {:subgroup s
                :best-edos (edo/best-in-subgroup size-range s)
                :viable-edos (map :edo (edo/in-subgroup size-range s))})]
    (save-edn "edos-by-subgroup" data)))

(defn compute-subgroups-by-edo []
  (save-edn "subgroups-by-edo"
            (for [n (range 15 100)]
              {:edo n
               :subgroups (-> (edo/as-temperament n [2 3 5 7 11 13])
                              temperament/errors-by-subgroup)})))

(defn score-temperament [s]
  (let [interval-weight (fn [r]
                          (/ (+ (/ 1 (interval/factor-sum r))
                                (/ 1 (interval/factor-sum (/ 2 r))))
                             2))
        periods-per-octave (/ 1200 (first (:generators s)))
        index-factor (fn [i]
                       (- 1 (* i periods-per-octave 1/24)))
        genchain-score (->> (:genchain s)
                            (map-indexed (fn [i rs]
                                           (* (index-factor i)
                                              (reduce + (map interval-weight rs)))))
                            (reduce +))
        error-penalty (* 1/5 (max 0 (- (:mean-error s) 5)))]
    (float (- genchain-score error-penalty))))

(comment
  (def t (temperament/named "tetracot"))

  (summary t)
  (save-edn "summaries" (map summary (temperament/load-all)))

  (temperament/optimize 20 t)
  (temperament/error-stats (temperament/flat-genchain 16 t) t)

  (temperament/genchain 22 t 10)

  (->> (edo/as-temperament 17 [2 3 13])
       (temperament/error-stats (interval/odd-limit 15)))

  (scale/chromatic-scale (edo/as-temperament 17 [2 3]))

  (compute-edos-by-subgroup)
  (compute-subgroups-by-edo)

  (->> (load-edn "summaries")
       (map (fn [s]
              {:name (:name s)
               :score (score-temperament s)}))
       (sort-by :score)
       reverse)

  :rcf)
