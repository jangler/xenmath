(ns results
  (:require [edo]
            [interval]
            [notation]
            [scale]
            [temperament :refer [error-stats]]
            [var]
            [clojure.math :as math]
            [clojure.set :refer [union]]))

(defn primes-in-ratios [rs]
  (reduce union (map interval/primes rs)))

(defn log2 [n]
  (/ (math/log n) (math/log 2)))

(defn edos-with-comma-steps [commas]
  (let [primes (primes-in-ratios commas)]
    (for [i (range 5 (int (* 2 (/ 1 (log2 (reduce min commas))))))]
      (let [t (edo/as-temperament i primes)
            es (error-stats (interval/subgroup 15 primes) t)]
        {:edo i
         :max-error (es :max-error)
         :mean-error (es :mean-error)
         :comma-steps (map #(first (temperament/tmap t %))
                           commas)}))))

(defn filter-comma-steps [commas pred]
  (->> (edos-with-comma-steps commas)
       (filter #(pred (:comma-steps %)))))

(defn all-edos-tempering-out [commas]
  (filter-comma-steps commas #(every? zero? %)))

(defn edos-tempering-out [commas]
  (edo/strip-nondecreasing-error (all-edos-tempering-out commas)))

(defn mos-report [[per gen]]
  (->> (scale/moses [per gen] [3 24])
       (map (fn [s]
              {:size (count s)
               :proper (scale/proper? s)}))))

(comment
  (mos-report [1200 176]) ; tetracot
  (mos-report [600 705]) ; srutal
  (mos-report [600 435]) ; echidna
  (mos-report [1200 443]) ; sensi
  (mos-report [1200 272]) ; orwell
  (mos-report [1200 317]) ; kleismic
  (mos-report [1200 193]) ; hemithirds
  (mos-report [1200 543]) ; joan
  (mos-report [1200 234]) ; slendric

  ; edos with comma mappings at most one step
  ; 31edo is by far the best in the 11-limit
  (->> (filter-comma-steps [81/80 64/63 33/32] #(= (reduce max %) 1))
       edo/strip-nondecreasing-error)

  ; edos where 81/80 = 64/63 and 33/32 = 1053/1024
  (->> (filter-comma-steps [81/80 64/63 33/32 1053/1024]
                           #(and (reduce = (take 2 %))
                                 (reduce = (drop 2 %))))
       edo/strip-nondecreasing-error)

  (->> (edos-with-comma-steps [81/80 64/63 33/32])
       (filter (fn [x]
                 (and (every? #(not (neg? %))
                              (x :comma-steps))
                      (< (x :max-error) 25)
                      (< (reduce + (x :comma-steps)) 6))))
       (group-by :comma-steps)
       vals
       (map #(first (sort-by :max-error %)))
       (sort-by #(reduce + (map abs (% :comma-steps)))))

  :rcf)

(defn fifth-tempering [& commas]
  (let [results (apply edos-tempering-out commas)]
    {:edos (map :edo results)
     :max-error (:max-error (last results))
     :fifth (->> (edo/as-temperament (:edo (last results))
                                     (primes-in-ratios commas))
                 (temperament/tuning 3/2)
                 float)}))

(comment
  (fifth-tempering 81/80)
  (fifth-tempering (/ 5/4 (* 9/8 2187/2048)))
  (fifth-tempering (/ 5/4 (/ 4/3 2187/2048)))
  (fifth-tempering 64/63)
  (fifth-tempering (/ (* 27/16 2187/2048) 7/4))
  (fifth-tempering (/ 7/4 (/ 16/9 2187/2048)))
  (fifth-tempering (/ 11/8 (* 2187/2048 81/64)))
  (fifth-tempering (/ (/ 3/2 2187/2048) 11/8))
  (compare 13/8 (/ 27/16 2187/2048))
  (fifth-tempering (/ 13/8 (/ 27/16 2187/2048)))
  (fifth-tempering (/ 13/8 (* 3/2 2187/2048)))
  (fifth-tempering (/ (/ 16/9 2187/2048) 13/8))
  (fifth-tempering 20480/19683 64/63)
  (fifth-tempering 20480/19683 8192/8019)
  (fifth-tempering 81/80 1053/1024)
  (fifth-tempering 32805/32768 180224/177147)
  (fifth-tempering 180224/177147 6656/6561)
  (fifth-tempering 180224/177147 262144/255879)
  (fifth-tempering 81/80 59049/57344 1053/1024)
  (fifth-tempering 1029/1024)
  :rcf)
