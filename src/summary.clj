(ns summary
  (:require [interval]
            [scale]
            [temperament]))

; TODO: edos supporting the temperament
(defn summary [t]
  (let [ms (scale/moses (:generators t) [5 24])
        es (temperament/error-stats (interval/odd-limit 15) t)]
    (conj t {:moses (map count ms)
             :proper-moses (map count (filter scale/proper? ms))
             :mean-error (:mean-error es)
             :genchain (->> (temperament/genchain (count (last ms)) t)
                            (map :ratios))})))
