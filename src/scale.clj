(ns scale)

(defn next-mode
  "Returns the next mode of a scale."
  [scale]
  (let [fst (first scale)
        lst (last scale)]
    (->> (concat (rest scale)
                 [(+ lst (first scale))])
         (map #(- % fst)))))

(defn mode
  "Returns the nth mode of a scale."
  [n scale]
  (if (zero? n)
    scale
    (mode (dec n) (next-mode scale))))

(defn modes
  "Returns all modes of the scale."
  [scale]
  (for [i (range (count scale))]
    (mode i scale)))

(defn near
  "Returns true if a is near b."
  [a b]
  (< (abs (- a b)) 1))

(defn step-sizes
  "Returns a set of step sizes in a scale."
  [scale]
  (reduce (fn [sizes size]
            (if (some #(near % size) sizes)
              sizes
              (conj sizes size)))
          #{}
          (map first (modes scale))))

(defn mos?
  "Returns true if a scale is a MOS scale."
  [scale]
  (= (count (step-sizes scale)) 2))

(defn proper?
  "Returns true if the scale is proper; that is, if the scale's interval sizes
   are ordered by their step spans."
  [scale]
  (loop [ms (modes scale)]
    (if (= (count (first ms)) 1)
      true
      (if (> (reduce max (map first ms))
             (reduce min (map second ms)))
        false
        (recur (map rest ms))))))

(defn chroma
  "Returns the chroma of a MOS scale; that is, the difference between its
   large and small steps."
  [scale]
  (->> (step-sizes scale)
       sort
       reverse
       (reduce -)))

(defn moses
  "Return MOS scales formed by a period and generator within a certain size
   range."
  [[per gen] [a b]]
  (loop [notes (for [i (range (/ 1200 per))]
                 (* (inc i) per))
         genpos 0
         found []]
    (if (> (count notes) b)
      found
      (let [genpos (mod (+ genpos gen) per)]
        (recur (sort (concat (for [i (range (/ 1200 per))]
                               (+ (* i per) genpos))
                             notes))
               genpos
               (if (and (mos? notes)
                        (>= (count notes) a))
                 (conj found notes)
                 found))))))

(defn step-counts
  "Return the [L s] step counts of a MOS scale."
  [scale]
  (let [sizes (reverse (sort (step-sizes scale)))
        steps (map first (modes scale))]
    (map (fn [size]
           (count (filter #(near % size) steps)))
         sizes)))

(defn pattern-name
  "Return the xL ys name of a MOS scale."
  [scale]
  (let [[L s] (step-counts scale)]
    (format "%dL %ds" L s)))

(defn viable-mos
  "Returns a linear temperament's closest MOS to 7 notes."
  ([t] (viable-mos t 8))
  ([t n]
   (loop [notes [0]
          candidate nil]
     (if (<= (count notes) n)
       (recur (conj notes (mod (+ (second (t :generators))
                                  (last notes))
                               1200))
              (if (and (mos? (conj (vec (sort notes)) 1200))
                       (or (nil? candidate)
                           (<= (abs (- (count notes) 7))
                               (abs (- (count candidate) 7)))))
                notes
                candidate))
       (conj (vec (sort candidate)) 1200)))))

(comment
  (def major-scale [200 400 500 700 900 1100 1200])
  (step-counts major-scale)
  (step-sizes major-scale)
  (modes major-scale)
  (mos? major-scale)
  (proper? major-scale)
  (def harmonic-minor-scale [200 300 500 700 800 1100 1200])
  (step-sizes harmonic-minor-scale)
  (mos? harmonic-minor-scale)
  (proper? harmonic-minor-scale) ; yes, but not strictly proper
  (def magic7 [322.8 380.7 703.4 761.4 1084.1 1142.1 1200])
  (step-sizes magic7)
  (modes magic7)
  (mos? magic7)
  (proper? magic7)
  (moses [1200 380.7] [5 9])
  (pattern-name magic7)
  :rcf)
