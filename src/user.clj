(ns user
  (:require [summary :refer [summary]]
            [search]
            [temperament]))

(comment
  (def t (temperament/named "diaschismic"))

  (summary t)
  (map summary (temperament/load-all))

  (def rs (temperament/flat-genchain 12 t))
  (temperament/optimize rs t)
  (temperament/error-stats rs t)

  (temperament/genchain 12 t)

  :rcf)
