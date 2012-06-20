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

(def white-figures #{ :R :N :B :Q :K :P })
(def black-figures #{ :r :n :b :q :k :p })

(defn white? [x] (contains? white-figures x))

(defn black? [x] (contains? black-figures x))

(defn figure-at "returns the figure keyword for the given board and coordinates"
 [board x y]
  (get-in board [:board (- 7 y) x ]))
(defn- set-figure [board x y figure]
  (assoc-in board [:board (- 7 y) x] figure))
(defn- pos-empty? "is a position on the given board empty"
  [board x y] (= :_ (figure-at board x y)))

(defn- pos-on-board? [x y]
  (every? (fn [n] (and (>= n 0) (<= n 7))) [x y]))

(defn enemy-on-pos? [board x y]
  (let [ fig-at-pos (figure-at board x y) ] 
  (if (= :w (:turn board))
    (black? fig-at-pos)
    (white? fig-at-pos))))
                        
(defn move-figure "moves a figure on the given board from x1,y2 to x2,y2"
  [board x1 y1 x2 y2]
  {:pre  [(pos-on-board? x1 y1)
          (pos-on-board? x2 y2)
          (not (pos-empty? board x1 y1))]}
  (let [fig (figure-at board x1 y1)]
    (set-figure (set-figure board x1 y1 :_) x2 y2 fig)))

(defn moves-vertical
  [x y c]
       (partition 2 (interleave (repeat x) c)))

(defn moves-down [x y]
  (moves-vertical x y (range (dec y) -1 -1)))

(defn moves-up [x y]
  (moves-vertical x y (range (inc y) 8)))

(defn moves-horizontal
  [x y c]
  (partition 2 (interleave c (repeat y))))

(defn moves-right [x y]
  (moves-horizontal x y (range (inc x) 8)))

(defn moves-left [x y]
  (moves-horizontal x y (range (dec x) -1 -1)))

