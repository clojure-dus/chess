(ns chess.core)

; UPPERCASE -> white
; lowercase -> black
(def initial-board
  {:board (vec (concat
                (vector [:r :n :b :q :k :b :n :r] (vec (repeat 8 :p)))
                (repeat 4 (vec (repeat 8 :_)))
                (vector (vec (repeat 8 :P)) [:R :N :B :Q :K :B :N :R]))),
   :rochade #{:K :Q :k :q},
   :turn :w})

(def white-figures #{:R :N :B :Q :K :P})
(def black-figures #{:r :n :b :q :k :p})

(defn white? [x] (contains? white-figures x))

(defn black? [x] (contains? black-figures x))

