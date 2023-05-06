(ns summary
  (:require [interval]
            [scale]
            [temperament]))

; TODO: edos supporting the temperament
(defn summary [t]
  (let [ms (scale/moses (:generators t) [5 24])
        gc (map :ratios (temperament/genchain (count (last ms)) t))
        es (temperament/error-stats (flatten gc) t)]
    (conj t {:moses (map count ms)
             :proper-moses (map count (filter scale/proper? ms))
             :mean-error (:mean-error es)
             :genchain gc})))
