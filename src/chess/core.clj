(ns chess.core)

; UPPERCASE -> white
; lowercase -> black
(def initial-board
  {:board (vec (concat
                (vector [:r :n :b :q :k :b :n :r] (vec (repeat 8 :p)))
                (repeat 4 (vec (repeat 8 :_)))
                (vector (vec (repeat 8 :P)) [:R :N :B :Q :K :B :N :R]))),
   :rochade #{:k :q :K :Q},
   :turn :w })

(defn figure-at "returns the figure keyword for the given board and coordinates"
 [board x y]
  (get-in board [:board x y]))

(defn- set-figure [board x y figure]
  (assoc-in board [:board x y] figure))

(defn- pos-empty? [board x y] (= :_ (figure-at board x y)))

(defn move-figure "moves a figure for the given board from x1,y2 to x2,y2"
  [board x1 y1 x2 y2]
  {:pre  [(every? (fn [i] (and (> i -1) (< i 8))) [x1 x2 y1 y2])
          (not (pos-empty? board x1 y1))]}
  (let [fig (figure-at board x1 y1)]
    (set-figure (set-figure board x1 y1 :_) x2 y2 fig)))


