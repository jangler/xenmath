(ns results
  (:require [notation :refer [all-notation]]
            [scale :refer [chroma viable-mos]]
            [temperament :refer [error-stats map-ratio]]
            [integer :refer [rational-power]]
            [search :refer [optimize]]
            [clojure.math :as math]
            [clojure.string :as str]))

(def odd-limit-15
  "Ratios we're interested in tuning accurately."
  [3/2 ; and 4/3
   9/8 ; and 16/9
   5/4 ; and 8/5
   5/3 ; and 6/5
   9/5 ; and 10/9
   15/8 ; and 16/15
   7/4 ; and 8/7
   7/6 ; and 12/7
   7/5 ; and 10/7
   9/7 ; and 14/9
   15/14 ; and 28/15
   11/6 ; and 12/11
   11/7 ; and 14/11
   11/8 ; and 16/11
   11/9 ; and 18/11
   11/10 ; and 20/11
   15/11 ; and 22/15
   13/7 ; and 14/13
   13/8 ; and 16/13
   13/9 ; and 18/13
   13/10 ; and 20/13
   13/11 ; and 22/13
   13/12 ; and 24/13
   15/13 ; and 26/15
   ])
(def no-elevens
  (filter #(= 0 (nth (integer/monzo %) (integer/prime-indices 11))) odd-limit-15))
(def no-13s
  (filter #(= 0 (nth (integer/monzo %) (integer/prime-indices 13))) odd-limit-15))
(def ol15-235
  (filter #(and (zero? (nth (integer/monzo %) (integer/prime-indices 11)))
                (zero? (nth (integer/monzo %) (integer/prime-indices 7))))
          odd-limit-15))
(def elevens-only
  (filter #(not= 0 (nth (integer/monzo %) (integer/prime-indices 11))) odd-limit-15))

(defn all-tunings
  "Return all tunings for a temperament."
  [t]
  (->> (concat odd-limit-15 (map #(* (/ 1 %) 2) odd-limit-15))
       (filter #(temperament/maps-ratio? t %))
       (sort-by (fn [r]
                  (reduce + (concat (integer/factors (numerator r))
                                    (integer/factors (denominator r))))))
       (map (fn [r]
              [r (temperament/linear-tuning r t)]))))

(def scale-ratios
  [256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2187/2048])

(defn scale-cents [t]
  (map (fn [r]
         [r (temperament/linear-tuning r t)])
       scale-ratios))

(defn factor-sum [r]
  (->> (integer/monzo r)
       (map abs)
       (map * [2 3 5 7 11 13])
       (reduce +)))

(->> no-13s
     (map #(vector % (notation/octave-reduce (/ 1 %))))
     flatten
     (sort-by factor-sum)
     (map (fn [r] {:ratio r :udn (notation/udn r)}))
     (map (fn [m]
            (format "%s in JI UDN,%s" (str (:ratio m)) (:udn m))))
     (clojure.string/join "\n")
     (spit "ji-udn-flashcards.csv"))

; meantone
(def septimal-meantone
  {:mapping [[1 0 -4 -13] [0 1 4 10]]
   :generators [1200 696.9521]})
(error-stats odd-limit-15 septimal-meantone)
(all-notation odd-limit-15 septimal-meantone 7 4 false)
(def godzilla
  {:mapping [[1 0 -4 2] [0 -2 -8 -1]]
   :generators [1200 252.635]})
(error-stats odd-limit-15 godzilla)
(viable-mos godzilla)
(all-notation odd-limit-15 godzilla 5 0 false)
(def undecimal-meantone
  {:mapping [[1 0 -4 -13 -25] [0 1 4 10 18]]
   :generators [1200 696.7130]})
(error-stats odd-limit-15 undecimal-meantone)
(def meanpop
  {:mapping [[1 0 -4 -13 24] [0 1 4 10 -13]]
   :generators [1200 696.5311]})
(error-stats odd-limit-15 meanpop)
(let [undecimal-notation (fn [t]
                           (->> (all-notation odd-limit-15 t 7 4 false #{1 4 5})
                                (filter #(not= 0 (nth (integer/monzo (% :ratio))
                                                      (integer/prime-indices 11))))))
      meantone-notation (undecimal-notation undecimal-meantone)
      meanpop-notation (undecimal-notation meanpop)]
  ; meanpop's error is a bit lower, but notation is a bit uglier.
  ; no mapping for 11/7 might be a dealbreaker, though.
  [meantone-notation meanpop-notation])

; (optimize undecimal-meantone 100000)
(all-notation [2187/2048 81/80 64/63 33/32] undecimal-meantone 7 4 false #{1 4 5})
(all-tunings undecimal-meantone)
(map (fn [r]
       [r (temperament/linear-tuning r undecimal-meantone)])
     [27/16 81/64 243/128 256/243 128/81 32/27 1024/729])
(scale-cents undecimal-meantone)

(def septimal-porcupine
  {:mapping [[1 2 3 2] [0 -3 -5 6]]
   :generators [1200 163.2032]})

(def srutal
  {:mapping [[2 0 11 -42] [0 1 -2 15]]
   :generators [600 704.814]})

(def pajara
  {:mapping [[2 2 7 8] [0 1 -2 -2]]
   :generators [600 706.843]})

(def magic
  {:mapping [[1 9 2 -1] [0 5 1 12]]
   :generators [1200 380.352]})

; edos
(def edo12
  {:mapping [[12 19 28]]
   :generators [(/ 1200 12)]})
(def edo19
  {:mapping [[19 30 44]]
   :generators [(/ 1200 19)]})
(def edo22
  {:mapping [[22 35 51 62 76]]
   :generators [(/ 1200 22)]})
(def edo27
  {:mapping [[27 43 63 76]]
   :generators [(/ 1200 27)]})
(def edo31
  {:mapping [[31 49 72 87 107]]
   :generators [(/ 1200 31)]})
(def edo41
  {:mapping [[41 65 95 115 142]]
   :generators [(/ 1200 41)]})
(def edo46
  {:mapping [[46 73 107 129]]
   :generators [(/ 1200 46)]})
(def edo53
  {:mapping [[53 84 123 149 183]]
   :generators [(/ 1200 53)]})
(def edo72
  {:mapping [[72 114 167 202 249]]
   :generators [(/ 1200 72)]})

(defn map-edo [n]
  (let [g (/ 1200 n)]
    [(vec (for [p [2 3 5 7 11]]
            (math/round (/ (temperament/cents-from-ratio p) g))))]))

(map #(search/map-interval (map-edo 49) %) [9/8 81/64 4/3 3/2 27/16 243/128 2187/2048])

(defn edo [n]
  {:mapping (map-edo n)
   :generators [(/ 1200 n)]})

(defn filter-limit [rs limit]
  (filter #(<= (reduce max (concat (integer/factors (numerator %))
                                   (integer/factors (denominator %))))
               limit)
          rs))

(defn edos-with-comma-steps [limit]
  (for [i (range 12 99)]
    (let [t (edo i)]
      {:edo i
       :max-error ((error-stats (filter-limit odd-limit-15 limit) t) :max-error)
       :comma-steps (map #(first (search/map-interval (t :mapping) %))
                         [81/80 64/63 33/32])})))

(defn strip-increasing-error [edos]
  (loop [remaining edos
         found []
         record 50]
    (if (empty? remaining)
      found
      (recur (rest remaining)
             (if (< (:max-error (first remaining)) record)
               (conj found (first remaining))
               found)
             (min record (:max-error (first remaining)))))))

(defn filter-comma-steps [limit pred]
  (->> (edos-with-comma-steps limit)
       (filter #(pred (:comma-steps %)))))

; septimal meantone
(->> (filter-comma-steps 7 #(zero? (first %)))
     strip-increasing-error)

; superpyth
(->> (filter-comma-steps 7 #(zero? (second %)))
     strip-increasing-error)

; hemifamity
(->> (filter-comma-steps 11 #(= % [1 1 2]))
     strip-increasing-error)

(->> (edos-with-comma-steps 11)
     (filter (fn [x]
               (and (every? #(not (neg? %))
                            (x :comma-steps))
                    (< (x :max-error) 25)
                    (< (reduce + (x :comma-steps)) 6))))
     (group-by :comma-steps)
     vals
     (map #(first (sort-by :max-error %)))
     (sort-by #(reduce + (map abs (% :comma-steps)))))

(def udn22
  {:mapping [[1 0 0 6 1]
             [0 1 0 -2 3]
             [0 0 1 0 -1]]
   :generators [1200 1908.7714022457055 2780.650963009264]})
(def udn31
  {:mapping [[1 0 -4 0 11]
             [0 1 4 0 -3]
             [0 0 0 1 -1]]
   :generators [1200 1896.5784357579885 3367.783512018541]})
(def udn41
  {:mapping [[1 0 0 10 -3]
             [0 1 0 -6 7]
             [0 0 1 1 -2]]
   :generators [1200 1902.9096324609097 2785.1226125354206]})
(for [t [(edo 22)
         udn22
         (edo 31)
         udn31
         (edo 41)
         udn41]]
  (:max-error (error-stats odd-limit-15 t)))

(for [r (map #(rational-power 12/7 %) (range 1 11))]
  (mod (first (search/map-interval (map-edo 46) r)) 46))

(defn log2 [n]
  (/ (math/log n) (math/log 2)))

(defn tenney-height [r]
  (log2 (* (numerator r) (denominator r))))

(->> no-13s
     (map #(vector % (notation/octave-reduce (/ 1 %))))
     flatten
     (sort-by tenney-height))

; meantone family
(error-stats odd-limit-15 septimal-meantone)
(error-stats odd-limit-15 edo12)
(error-stats odd-limit-15 edo19)
(error-stats odd-limit-15 edo31)

; porcupine family
(error-stats odd-limit-15 septimal-porcupine)
(error-stats odd-limit-15 edo22)

; diaschismic family
(error-stats odd-limit-15 srutal)
(error-stats odd-limit-15 pajara)

; archytas clan
(def superpyth
  {:mapping [[1 2 6 2] [0 -1 -9 2]]
   :generators [1200 490.4096]})
(error-stats odd-limit-15 superpyth)
(scale/moses (superpyth :generators) [7 7])
(all-notation odd-limit-15 superpyth 7 1 true #{1 4 5})
(scale-cents superpyth)
(def archy
  {:mapping [[1 0 nil 6] [0 1 nil -2]]
   :generators [1200 709.321]})
(error-stats odd-limit-15 archy)
(viable-mos archy)
(all-notation odd-limit-15 archy 7 2 true)
(def beatles
  {:mapping [[1 1 nil 4] [0 2 nil -4]]
   :generators [1200 354.6606]})
(error-stats odd-limit-15 beatles)
(viable-mos beatles)
(all-notation odd-limit-15 beatles 7 4 true)

; magic
(error-stats odd-limit-15 magic)

; semicomma family
(def orwell
  {:mapping [[1 0 3 1 3] [0 7 -3 8 2]]
   :generators [1200 271.137]})
; (optimize orwell 100000)
(error-stats odd-limit-15 orwell)
(error-stats odd-limit-15 edo53)

; sensi
(def septimal-sensi
  {:mapping [[1 -1 -1 -2] [0 7 9 13]]
   :generators [1200 443.5193]})
(error-stats odd-limit-15 septimal-sensi)
(chroma (viable-mos septimal-sensi))
(all-notation odd-limit-15 septimal-sensi 8 5 true #{1})
(def sensation
  {:mapping [[1 -1 -1 -2 nil 0] [0 7 9 13 nil 10]]
   :generators [1200 443.322]})
(error-stats odd-limit-15 sensation)
(all-notation odd-limit-15 sensation 8 5 true #{1})

; tetracot
(def tetracot
  {:mapping [[1 1 1 5 2] [0 4 9 -15 10]]
   :generators [1200 175.570]})
(error-stats odd-limit-15 tetracot)
(viable-mos tetracot)
(all-notation odd-limit-15 tetracot 7 0 false)
(error-stats odd-limit-15 edo41)
(def octacot
  {:mapping [[1 1 1 2 2] [0 8 18 11 20]]
   :generators [1200 87.9751]})
(error-stats odd-limit-15 octacot)
(viable-mos octacot)

; kleismic
(def catakleismic
  {:mapping [[1 0 1 -3 nil 0] [0 6 5 22 nil 14]]
   :generators [1200 316.732]})
(error-stats odd-limit-15 catakleismic)
(viable-mos catakleismic)
(all-notation odd-limit-15 catakleismic 7 5 true)
(def edo53-7-limit
  {:mapping [[53 31 17 43]]
   :generators [(/ 1200 53)]})
(error-stats odd-limit-15 edo53-7-limit)
(error-stats odd-limit-15 edo72)
(def keemun
  {:mapping [[1 0 1 2] [0 6 5 3]]
   :generators [1200 316.473]})
(error-stats odd-limit-15 keemun)
(viable-mos keemun)

; orwell
(viable-mos orwell)
(def orwell9 (viable-mos orwell))
(chroma orwell9)
(all-notation odd-limit-15 orwell 9 0 false #{1})

; miracle
(def miracle
  {:mapping [[1 1 3 3] [0 6 -7 -2]]
   :generators [1200 116.675]})
(error-stats odd-limit-15 miracle)
(viable-mos miracle 8)

; myna
(def myna
  {:mapping [[1 -9 -9 -8] [0 10 9 7]]
   :generators [1200 310.146]})
(error-stats odd-limit-15 myna)
(viable-mos myna) ; not nice

; garibaldi
(def garibaldi
  {:mapping [[1 0 15 25] [0 1 -8 -14]]
   :generators [1200 702.085]})
(error-stats odd-limit-15 garibaldi)
(viable-mos garibaldi)
(all-notation odd-limit-15 garibaldi 7 0 false)

; gamelismic clan
(def slendric
  {:mapping [[1 1 nil 3] [0 3 nil -1]]
   :generators [1200 233.688]})
(error-stats odd-limit-15 slendric)
(->> (viable-mos slendric 5)
     (chroma))
(all-notation odd-limit-15 slendric 5 3 true) ; wrong result currently

; liese
(def liese
  {:mapping [[1 3 nil 8] [0 -3 nil -11]]
   :generators [1200 566.4752]})
(error-stats odd-limit-15 liese)
(viable-mos liese)

; squares
(def squares
  {:mapping [[1 3 nil 6] [0 -4 nil -9]]
   :generators [1200 425.3655]})
(error-stats odd-limit-15 squares)
(viable-mos squares)
(all-notation odd-limit-15 squares 8 5 true)

; marvel
(def marvel
  {:mapping [[1 0 0 -5 12] [0 1 0 2 -1] [0 0 1 2 -3]]
   :generators [1200 700.5584 383.8545]})
; (optimize marvel 100000)
(temperament/linear-tuning 81/80 marvel)
(def marvel-syncom
  {:mapping [[1 0 0 -5 12] [0 1 4 10 -13] [0 0 -1 -2 3]]
   :generators [1200 700.5584 18.3791]})
(def minerva
  {:mapping [[1 0 0 -5 -9] [0 1 0 2 2] [0 0 1 2 4]]
   :generators [1200 700.7617 386.8520]})
(temperament/linear-tuning 81/80 minerva)
; (optimize minerva 100000)
(def minerva-syncom ; syntonic comma as third generator
  {:mapping [[1 0 -2 -5 -3] [0 1 4 10 18] [0 0 -1 -2 -4]]
   :generators [1200 700.7617 16.1948]})
(error-stats odd-limit-15 marvel)
(error-stats odd-limit-15 marvel-syncom)
(error-stats odd-limit-15 minerva)
(error-stats odd-limit-15 minerva-syncom) ; a bit higher for some reason?
(temperament/map-ratio 11/8 minerva-syncom)
(defn all-notation-planar
  "Return notation for a planar temperament. The second generator must be a
   fifth, and the third generator must be a comma."
  [t]
  (->> (concat odd-limit-15 (map #(* (/ 1 %) 2) odd-limit-15))
       (filter #(temperament/maps-ratio? t %))
       (sort-by (fn [r]
                  (reduce + (concat (integer/factors (numerator r))
                                    (integer/factors (denominator r))))))
       (map (fn [r]
              [r (notation/notate-planar r t)]))))
; (all-notation-planar marvel-syncom) ; -4!
(all-notation-planar minerva-syncom) ; for my purposes, this is v ugly

; hemifamity
(def hemifamity
  {:mapping [[1 0 0 10] [0 1 0 -6] [0 0 1 1]]
   :generators [1200 1902.8292 2786.8177]})
(error-stats odd-limit-15 hemifamity)
(def pele
  {:mapping [[1 0 0 10 17] [0 1 0 -6 -10] [0 0 1 1 1]]
   :generators [1200 1903.3592 2788.6230]})
(error-stats odd-limit-15 pele)
; (optimize pele 100000)
(def laka
  {:mapping [[1 0 0 10 -18] [0 1 0 -6 15] [0 0 1 1 -1]]
   :generators [1200 1902.6246 2786.3153]})
(error-stats odd-limit-15 laka)
; (optimize laka 100000)
(def akea
  {:mapping [[1 0 0 10 -3] [0 1 0 -6 7] [0 0 1 1 -2]]
   :generators [1200 1902.9138 2785.1307]})
(error-stats odd-limit-15 akea)
; (optimize akea 100000)
(def hemifamity-comma
  {:mapping [[1 0 -2 4] [0 1 4 -2] [0 0 -1 -1]]
   :generators [1200 702.6752 24.3852]})
(error-stats odd-limit-15 hemifamity-comma)
; (optimize hemifamity-comma 100000)
(all-notation-planar hemifamity-comma)
(def pele-comma
  {:mapping [[1 0 -2 4 0] [0 1 4 -2 -6] [0 0 -1 -1 -1]]
   :generators [1200 1903.3592 24.8138]})
(error-stats odd-limit-15 pele-comma)
(->> (range -20 20) ; to find mappings
     (map (fn [i]
            {:mapping [[1 0 -2 4 0] [0 1 4 -2 i] [0 0 -1 -1 -1]]
             :generators [1200 1903.3592 24.8138]}))
     (sort-by #((error-stats odd-limit-15 %) :max-error)))
(all-notation-planar pele-comma)
(def laka-comma
  {:mapping [[1 0 -2 4 0] [0 1 4 -2 11] [0 0 -1 -1 1]]
   :generators [1200 1902.6246 24.1832]})
(error-stats odd-limit-15 laka-comma)
; (optimize laka-comma 100000)
(all-notation-planar laka-comma)
(def akea-comma
  {:mapping [[1 0 -2 4 0] [0 1 4 -2 -1] [0 0 -1 -1 2]]
   :generators [1200 702.6774 24.3979]})
(defn error-func-weight-7-limit [t]
  (let [es11 (error-stats elevens-only t)
        es7 (error-stats no-elevens t)]
    (+ (es11 :max-error) (* 3 (es7 :max-error)))))
; (optimize akea-comma 10000 error-func-weight-7-limit)
(error-stats odd-limit-15 akea-comma)
(all-notation-planar akea-comma)
(map (fn [r]
       [r (temperament/linear-tuning r laka-comma)])
     [27/16 81/64 243/128 256/243 128/81 32/27 45/32])
(def hemifamity-11 ; adds a "generator" for the undecimal comma
  {:mapping [[1 0 -2 4 1] [0 1 4 -2 -1] [0 0 -1 -1 0] [0 0 0 0 1]]
   :generators [1200 702.6743 24.3867 54.1693]})
(error-stats odd-limit-15 hemifamity-11)
; (optimize hemifamity-11 100000)
(all-notation-planar hemifamity-11)
(all-tunings pele-comma)
(map (fn [r]
       [r (temperament/linear-tuning r pele-comma)])
     [27/16 81/64 243/128 256/243 128/81 32/27 1024/729 81/80 2187/2048])
(let [undecimal-notation (fn [t]
                           (->> (all-notation-planar t)
                                (filter #(not= 0 (nth (integer/monzo (first %))
                                                      (integer/prime-indices 11))))))]
  {:pele (undecimal-notation pele-comma)
   :laka (undecimal-notation laka-comma)
   :h11  (undecimal-notation hemifamity-11)
   :akea (undecimal-notation akea-comma)})
(let [tuning #(temperament/linear-tuning % hemifamity-11)
      yo-third (tuning 5/4)
      wa-third (tuning 81/64)
      fifth (tuning 3/2)]
  {:sharp-flat (mod (* fifth 7) 1200)
   :up-down (- wa-third yo-third)})

; schismatic family
(def andromeda
  {:mapping [[1 2 -1 -3 -4] [0 -1 8 14 18]]
   :generators [1200 497.6286]})
(error-stats odd-limit-15 andromeda)
(viable-mos andromeda)
(all-notation odd-limit-15 andromeda 7 1 true #{1 4 5})

; starling temperaments
(def nusecond
  {:mapping [[1 3 4 5 5] [0 -11 -13 -17 -12]]
   :generators [1200 154.7408]})
; (optimize nusecond 100000)
(error-stats odd-limit-15 nusecond)
(viable-mos nusecond)
(all-notation odd-limit-15 nusecond 7 6 true #{1})
(all-notation odd-limit-15 nusecond 8 0 false #{1})

(def superkleismic
  {:mapping [[1 4 5 2 4] [0 -9 -10 3 -2]]
   :generators [1200 321.8466]})
(error-stats odd-limit-15 superkleismic)
(viable-mos superkleismic)

; dicot family
; i don't think it makes musical sense to call this a 2.3.5 temperament.
; these generators would make a great 2.3.11 temperament, though, and that
; temperament is "neutral".
(def dicot
  {:mapping [[1 1 2] [0 2 1]]
   :generators [1200 350.9775]})
(error-stats odd-limit-15 dicot)
(viable-mos dicot)

; neutral
(def neutral
  {:mapping [[1 1 nil nil 2] [0 2 nil nil 5]]
   :generators [1200 350.2636]})
(error-stats odd-limit-15 neutral)
(viable-mos neutral)

; slendro clan
(def semaphore
  {:mapping [[1 2 nil 3] [0 -2 nil -1]]
   :generators [1200 249.0226]})
(error-stats odd-limit-15 semaphore)
(viable-mos semaphore 9)

; mavila
(def mavila
  {:mapping [[1 2 1] [0 -1 3]]
   :generators [1200 521.0896]})
(error-stats odd-limit-15 mavila)

; bleu
(def bleu
  {:name "bleu[9]",
   :mapping [[1 1 nil 2 3 nil] [0 5 nil 7 4 nil]],
   :generators [1200 140.39100356000483],
   :mos-size 9,
   :pattern "8L 1s"})
(error-stats odd-limit-15 bleu)
(all-notation odd-limit-15 bleu 9 0 false #{1})
