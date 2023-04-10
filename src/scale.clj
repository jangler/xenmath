(ns scale)

(defn step-sizes
  "Returns a set of step sizes in a scale."
  [scale]
  (loop [sizes #{}
         remaining scale]
    (if (< (count remaining) 2)
      sizes
      (recur (let [size (- (first remaining) (second remaining))]
               (if (not-any? #(< (abs (- size %)) 1) sizes)
                 (conj sizes size)
                 sizes))
             (rest remaining)))))

(defn mos?
  "Returns true if a scale is a MOS scale."
  [scale]
  (= (count (step-sizes scale)) 2))

(defn chroma
  "Returns the chroma of a MOS scale; that is, the difference between its
   large and small steps."
  [scale]
  (->> (step-sizes scale)
       sort
       reverse
       (reduce -)))

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
