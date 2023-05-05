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

(comment
  (def t (temperament/named "diaschismic"))

  (summary t)
  (map summary (temperament/load-all))

  (temperament/optimize 20 t)
  (temperament/error-stats (temperament/flat-genchain 20 t) t)

  (temperament/genchain 20 t)

  (->> (edo/as-temperament 17 [2 3 13])
       (temperament/error-stats (interval/odd-limit 15)))

  (scale/chromatic-scale (edo/as-temperament 17 [2 3]))

  (compute-edos-by-subgroup)
  (compute-subgroups-by-edo)

  :rcf)
