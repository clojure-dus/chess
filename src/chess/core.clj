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

(defn steps-vertical
  [x y c]
       (partition 2 (interleave (repeat x) c)))

(defn steps-down [x y]
  (steps-vertical x y (range (dec y) -1 -1)))

(defn steps-up [x y]
  (steps-vertical x y (range (inc y) 8)))

(defn steps-horizontal
  [x y c]
  (partition 2 (interleave c (repeat y))))

(defn steps-right [x y]
  (steps-horizontal x y (range (inc x) 8)))

(defn steps-left [x y]
  (steps-horizontal x y (range (dec x) -1 -1)))

(defn steps-diagonal [fx fy x y]
  (take-while #(apply pos-on-board? %) (drop 1 (iterate (fn [[a b]] (list (fx a) (fy b))) [x y]))))

(defn empty-moves [f board x y]
  (take-while (fn [[a b]] (pos-empty? board a b)) (f x y)))

(defn attacking-moves [f board x y]
  (first (drop-while (fn [[a b]] (pos-empty? board a b)) (f x y))))

(defn fetch-direction [figure]
  (let [diag steps-diagonal]
    (if (white? figure)
      { :up steps-up :down steps-down :left steps-left :right steps-right :up-left (partial diag dec inc) :up-right (partial diag inc inc) :down-left (partial diag dec dec) :down-right (partial diag inc dec) }
      { :up steps-down :down steps-up :left steps-right :right steps-left :up-left (partial diag inc dec) :up-right (partial diag dec dec) :down-left (partial diag inc inc) :down-right (partial diag dec inc) })))

(defn steps-without-attack [ board x y k n ]
  (let [ dir-fn (k (fetch-direction (figure-at board x y))) ]
    (take n (empty-moves dir-fn board x y))))

(defn steps-with-attack [ board x y k n ]
  (let [ figure (figure-at board x y) dir-fn (k (fetch-direction figure)) steps (take n (dir-fn x y))
        enemy (first (filter (fn [[a b]] (enemy-on-pos? board a b)) steps))]
    (when (every? (fn [[a b]] (= :_ (figure-at board a b))) (take-while #(not (= % enemy)) steps)) enemy)))

(defn pawn-moves [board x y]
  {:pre [(contains? #{:P :p} (figure-at board x y))]}
  (let [non-attacks (partial steps-without-attack board x y) attacks (partial steps-with-attack board x y)]
  (concat #{}
          (cond (and (white? (figure-at board x y)) (= y 1)) (non-attacks :up 2)
                (and (black? (figure-at board x y)) (= y 6)) (non-attacks :up 2)
                :else (non-attacks :up 1))
          (attacks :up-right 1) 
          (attacks :up-left  1))))

(defn queen-moves [board x y]
  {:pre [(contains? #{:Q :q} (figure-at board x y))]}
  (let [non-attacks (partial steps-without-attack board x y) attacks (partial steps-with-attack board x y)]
    (map #(attacks % 8) '(:up-left :up-right :down-left :down-right :up :down :left :right))))




  
         