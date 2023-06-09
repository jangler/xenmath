(ns temperament
  (:require [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.set :as set]
            [interval]
            [number]
            [var]))

(def store-path
  "File path where temperaments are stored."
  "data/temperaments.edn")

(defn load-all
  "Loads all temperaments from the store."
  []
  (edn/read-string (slurp store-path)))

(defn named
  "Return a temperament with the given name, or nil."
  [name]
  (first (filter #(= (:name %) name) (load-all))))

(defn tmap
  "Returns the generator exponent vector for interval r in temperament t.
   If the temperament does not map the ratio, returns nil."
  [t r]
  (let [m (:mapping t)
        v (interval/monzo r)]
    (if (every? (fn [[i x]]
                  (or (zero? x)
                      (and (contains? (first m) i)
                           (some? ((first m) i)))))
                (map-indexed vector v))
      (->> m
           (map (fn [xs]
                  (map #(if (nil? %) 0 %) xs)))
           (map #(reduce + (map * v %))))
      nil)))

(defn maps?
  "Returns true if temperament t maps interval r."
  [t r]
  (some? (tmap t r)))

(defn edosteps
  "Returns the steps to interval r in rank-1 temperament t."
  [t r]
  (first (tmap t r)))

(defn tuning
  "Returns the cents of interval r in temperament t."
  [r t]
  (some->> (tmap t r)
           (map * (:generators t))
           (reduce +)))

(defn tuning-error
  "Returns the tuning error of ratio r in temperament t."
  [r t]
  (let [c (tuning r t)]
    (if (nil? c)
      nil
      (- c (interval/cents r)))))

(defn error-stats
  "Returns a map of error stats for intervals rs in temperament t."
  [rs t]
  (let [round #(number/places 4 %)
        errors (into {} (keep (fn [r]
                                (some->> (tuning-error r t)
                                         round
                                         (vector r)))
                              rs))
        abs-errors (map abs (vals errors))]
    {:errors errors
     :mean-error (if (not-empty abs-errors)
                   (round (/ (reduce + abs-errors) (count abs-errors)))
                   nil)
     :max-error (if (not-empty abs-errors)
                  (reduce max abs-errors)
                  nil)}))

(defn tmap-generator-all
  "Return a map of mappings to ratios from rs in temperament t.
   Only the generator mapping is included."
  [t rs]
  (-> (group-by #(second (tmap t %)) rs)
      (dissoc nil)))

(defn genchain
  "Return the genchain of temperament t to n notes, listing potential
   unmapped intervals with less than tolerance cents of error (default 15)."
  ([n t] (genchain n t 15))
  ([n t tolerance]
   (let [gs (:generators t)
         rs (interval/odd-limit var/*odd-limit*)
         ms (tmap-generator-all t rs)
         per (first gs)
         ppo (/ 1200 per)]
     (for [i (range (/ n ppo))]
       (let [cs (for [j (range ppo)]
                  (mod (+ (* per j) (* (second gs) i))
                       1200))]
         {:cents cs
          :ratios (or (ms i) [])
          :also-near (filter (fn [r]
                               (let [c (interval/cents r)]
                                 (and (not-any? #(= % r) (ms i))
                                      (some #(< (abs (- c %)) tolerance) cs))))
                             rs)})))))

(defn flat-genchain
  "Return only the intervals in the genchain of temperament t to n notes."
  [n t]
  (flatten (map :ratios (genchain n t))))

(defn optimize
  "Return temperament t's generators optimized for the intervals in a genchain
   of length n."
  ([n t] (optimize n t false))
  ([n t half-sharp?]
   (let [rs (flat-genchain n t)
         errfn #(let [es (error-stats rs %)]
                  (+ (:max-error es)
                     (* 0.1 (:mean-error es))))]
     (loop [t t
            n 10000
            e (errfn t)]
       (if (pos? n)
         (let [gs (t :generators)
               t2 (assoc t :generators
                         (vec (concat [(first gs)]
                                      (->> (rest gs)
                                           (map #(+ % (- (math/random) 0.5)))))))
               gs (t2 :generators)
               t2 (if half-sharp?
                    (assoc t2 :generators (vector (first gs)
                                                  (second gs)
                                                  (mod (* (second gs) 7) 1200)))
                    t2)
               e2 (errfn t2)]
           (recur (if (< e2 e) t2 t)
                  (dec n)
                  (if (< e2 e) e2 e)))
         (map #(number/places 4 %) (:generators t)))))))

(defn errors-by-subgroup
  [t]
  (->> (for [s (number/viable-subgroups)]
         (conj {:subgroup s}
               (dissoc (error-stats (interval/subgroup var/*odd-limit* s) t)
                       :errors)))
       (filter #(< (:max-error %) 15))
       (sort-by :max-error)))

(defn primes
  "Returns primes mapped by temperament t."
  [t]
  (let [m (set/map-invert number/prime-indices)]
    (->> (first (t :mapping))
         (keep-indexed (fn [i x]
                         (if (some? x) (m i) nil))))))
