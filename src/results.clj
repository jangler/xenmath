(ns results
  (:require [edo]
            [interval]
            [notation]
            [scale]
            [temperament :refer [error-stats]]
            [var]
            [clojure.math :as math]
            [clojure.set :refer [union]]))

(defn notate-edo
  "Return a list of valid notation strings for ratio r in edo n."
  [n r]
  (let [t (edo/as-temperament n (interval/primes r))
        naturals (map-indexed (fn [i r]
                                {:degree (inc i)
                                 :steps (first (temperament/tmap t r))
                                 :sharps 0
                                 :ups 0})
                              [1/1 9/8 81/64 4/3 3/2 27/16 243/128 2/1])
        apotome-steps (first (temperament/tmap t 2187/2048))
        add-sharp (fn [n i]
                    (-> n
                        (update :sharps #(+ % i))
                        (update :steps #(+ % (* i apotome-steps)))))
        add-up (fn [n i]
                 (-> n
                     (update :ups #(+ % i))
                     (update :steps #(+ % i))))
        notes (mapcat (fn [n]
                        (for [sharps (if (#{1 4 5 8} (:degree n))
                                       [-1 0 1]
                                       [-2 -1 0 1])
                              ups [-1 0 1]]
                          (add-sharp (add-up n ups) sharps)))
                      naturals)
        r-steps (first (temperament/tmap t r))]
    (->> notes
         (filter #(= r-steps (:steps %)))
         (map (fn [n]
                (let [quality (if (#{1 4 5 8} (:degree n))
                                (case (:sharps n)
                                  -1 "d"
                                  0 "P"
                                  1 "A")
                                (case (:sharps n)
                                  -2 "d"
                                  -1 "m"
                                  0 "M"
                                  1 "A"))
                      downs (apply str (map (fn [_] "v") (range (- (:ups n)))))
                      ups (apply str (map (fn [_] "^") (range (:ups n))))]
                  (str ups downs quality (:degree n))))))))

(defn strip-degree [s]
  (subs s 0 (dec (count s))))

(defn primes-in-ratios [rs]
  (reduce union (map interval/primes rs)))

(defn edo-interval-quality [rs]
  (let [edos (filter (fn [n]
                       (let [ps (primes-in-ratios rs)
                             t (edo/as-temperament n ps)]
                         (every? #(< (abs (temperament/tuning-error % t)) 20) rs)))
                     (range 19 54))
        quality-set (fn [n]
                      (set (mapcat (fn [r]
                                     (map strip-degree (notate-edo n r))) rs)))
        ns (->> (for [n edos]
                  (vec (quality-set n)))
                flatten
                set
                (map (fn [n]
                       {:notation n
                        :edos (filter #(contains? (quality-set %) n) edos)}))
                (sort-by (comp count :edos))
                reverse)]
    (->> (loop [ns ns
                found-edos #{}
                results []]
           (if (empty? ns)
             results
             (recur (rest ns)
                    (union found-edos (set (:edos (first ns))))
                    (conj results (update (first ns) :edos (fn [xs]
                                                             (filter #(not (found-edos %))
                                                                     xs)))))))
         (filter (comp not empty? :edos)))))

(comment
  ; 5-over
  (edo-interval-quality [10/9 5/4 5/3 15/8])
  ; 7-over
  (edo-interval-quality [7/6 7/4 14/9])
  ; 7/5
  (edo-interval-quality [7/5])
  ; 11-over
  (edo-interval-quality [11/9 11/6])
  (map #(notate-edo 49 %) [5/4 8/5 7/4 8/7 11/9 18/11])
  :rcf)

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

(defn all-notation-planar
  "Return notation for a planar temperament. The second generator must be a
   fifth, and the third generator must be a comma."
  [t]
  (->> (interval/odd-limit var/*odd-limit*)
       (filter #(temperament/maps? t %))
       (sort-by interval/factor-sum)
       (map (fn [r]
              [r (notation/notate-planar r t)]))))
