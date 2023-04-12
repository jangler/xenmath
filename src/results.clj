(ns results
  (:require [notation :refer [all-notation]]
            [scale :refer [chroma viable-mos]]
            [temperament :refer [error-stats map-ratio]]
            [integer]
            [clojure.math :as math]))

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
(defn optimize
  [t rs n]
  (let [error (fn [t]
                (let [es (error-stats rs t)]
                  (es :max-error)))]
    (loop [t t
           n n
           e (error t)]
      (if (pos? n)
        (let [gs (t :generators)
              t2 (assoc t :generators
                        (vec (concat [(first gs)]
                                     (->> (rest gs)
                                          (map #(+ % (- (math/random) 0.5)))))))
              e2 (error t2)]
          (recur (if (< e2 e) t2 t)
                 (dec n)
                 (if (< e2 e) e2 e)))
        t))))
(optimize undecimal-meantone odd-limit-15 100000)

(def edo12
  {:mapping [[12 7 4 10]]
   :generators [(/ 1200 12)]})

(def edo19
  {:mapping [[19 11 6 15]]
   :generators [(/ 1200 19)]})

(def edo31
  {:mapping [[31 18 10 25]]
   :generators [(/ 1200 31)]})

(def edo72
  {:mapping [[72 42 23 58]]
   :generators [(/ 1200 72)]})

(def septimal-porcupine
  {:mapping [[1 2 3 2] [0 -3 -5 6]]
   :generators [1200 163.2032]})

(def edo22
  {:mapping [[22 13 7 18]]
   :generators [(/ 1200 22)]})

(def srutal
  {:mapping [[2 0 11 -42] [0 1 -2 15]]
   :generators [600 704.814]})

(def pajara
  {:mapping [[2 2 7 8] [0 1 -2 -2]]
   :generators [600 706.843]})

(def magic
  {:mapping [[1 9 2 -1] [0 5 1 12]]
   :generators [1200 380.352]})

(def orwell
  {:mapping [[1 0 3 1 3] [0 7 -3 8 2]]
   :generators [1200 271.426]})

(def edo53
  {:mapping [[53 31 17 43 24]]
   :generators [(/ 1200 53)]})

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
  {:mapping [[1 0 -12 6] [0 1 9 -2]]
   :generators [1200 710.291]})
(error-stats odd-limit-15 superpyth)
(def archy
  {:mapping [[1 0 nil 6] [0 1 nil -2]]
   :generators [1200 709.321]})
(error-stats odd-limit-15 archy)
(viable-mos archy)
(all-notation odd-limit-15 archy 7 6 true)
(def beatles
  {:mapping [[1 1 nil 4] [0 2 nil -4]]
   :generators [1200 354.6606]})
(error-stats odd-limit-15 beatles)
(viable-mos beatles)
(all-notation odd-limit-15 beatles 7 4 true)

; magic
(error-stats odd-limit-15 magic)

; semicomma family
(error-stats odd-limit-15 orwell)
(error-stats odd-limit-15 edo53)

; sensi
(def septimal-sensi
  {:mapping [[1 -1 -1 -2] [0 7 9 13]]
   :generators [1200 443.383]})
(error-stats odd-limit-15 septimal-sensi)
(chroma (viable-mos septimal-sensi))
(all-notation odd-limit-15 septimal-sensi 8 5 true)
(def sensation
  {:mapping [[1 -1 -1 -2 nil 0] [0 7 9 13 nil 10]]
   :generators [1200 443.322]})
(error-stats odd-limit-15 sensation)
(all-notation odd-limit-15 sensation 8 5 true)
(def edo46
  {:mapping [[46 27 15 37]]
   :generators [(/ 1200 46)]})
(error-stats odd-limit-15 edo46)

; tetracot
(def tetracot
  {:mapping [[1 1 1 5 2] [0 4 9 -15 10]]
   :generators [1200 175.570]})
(error-stats odd-limit-15 tetracot)
(viable-mos tetracot)
(all-notation odd-limit-15 tetracot 7 0 false)
(def edo41
  {:mapping [[41 24 13 33 19]]
   :generators [(/ 1200 41)]})
(error-stats odd-limit-15 edo41)

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
(def orwell9 (viable-mos orwell))
(chroma orwell9)
(all-notation odd-limit-15 orwell 9 0 false)

; miracle
(def miracle
  {:mapping [[1 1 3 3] [0 6 -7 -2]]
   :generators [1200 116.675]})
(error-stats odd-limit-15 miracle)
(viable-mos miracle) ; doesn't give a nice result currently

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
  {:mapping [[1 0 0 -5] [0 1 0 2] [0 0 1 2]]
   :generators [1200 700.4075 383.6376]})
(def minerva
  {:mapping [[1 0 0 -5 -9] [0 1 0 2 2] [0 0 1 2 4]]
   :generators [1200 700.2593 386.5581]})
(def minerva-syncom ; syntonic comma as third generator
  {:mapping [[1 0 -2 -5 -3] [0 1 4 10 6] [0 0 -1 -2 -3]]
   :generators [1200 700.2593 14.4791]})
(error-stats odd-limit-15 marvel)
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
(all-notation-planar minerva-syncom)

; hemifamity
(def hemifamity
  {:mapping [[1 0 0 10] [0 1 0 -6] [0 0 1 1]]
   :generators [1200 1902.8292 2786.8177]})
(error-stats odd-limit-15 hemifamity)
(let [tuning #(temperament/linear-tuning % hemifamity)
      yo-third (tuning 5/4)
      wa-third (tuning 81/64)
      fifth (tuning 3/2)]
  {:sharp-flat (mod (* fifth 7) 1200)
   :up-down (- wa-third yo-third)})
(def hemifamity-comma
  {:mapping [[1 0 -2 4] [0 1 4 -2] [0 0 -1 -1]]
   :generators [1200 702.6752 24.3852]})
(error-stats odd-limit-15 hemifamity-comma)
(optimize hemifamity-comma odd-limit-15 100000)
(all-notation-planar hemifamity-comma)
(all-tunings hemifamity)
(map (fn [r]
       [r (temperament/linear-tuning r hemifamity)])
     [27/16 81/64 243/128 256/243 128/81 32/27 45/32])
