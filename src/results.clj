(ns results
  (:require [notation :refer [all-notation]]
            [scale :refer [chroma viable-mos]]
            [temperament :refer [error-stats]]))

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

(def septimal-meantone
  {:mapping [[1 0 -4 -13] [0 1 4 10]]
   :generators [1200 696.9521]})

(def edo12
  {:mapping [[12 7 4 10]]
   :generators [(/ 1200 12)]})

(def edo19
  {:mapping [[19 11 6 15]]
   :generators [(/ 1200 19)]})

(def edo31
  {:mapping [[31 18 10 25]]
   :generators [(/ 1200 31)]})

(def marvel
  {:mapping [[1 0 0 -5] [0 1 0 2] [0 0 1 2]]
   :generators [1200 700.4075 383.6376]})

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

(def superpyth
  {:mapping [[1 0 -12 6] [0 1 9 -2]]
   :generators [1200 710.291]})

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

; marvel family
(error-stats odd-limit-15 marvel)
(error-stats odd-limit-15 edo72)

; porcupine family
(error-stats odd-limit-15 septimal-porcupine)
(error-stats odd-limit-15 edo22)

; diaschismic family
(error-stats odd-limit-15 srutal)
(error-stats odd-limit-15 pajara)

; superpyth
(error-stats odd-limit-15 superpyth)

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

; catakleismic
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

(def meantone7 (viable-mos septimal-meantone))
(chroma meantone7)
(all-notation odd-limit-15 septimal-meantone 7 4 false)
