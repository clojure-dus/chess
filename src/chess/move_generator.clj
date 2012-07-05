(ns chess.move-generator
  (:use [chess.core :only (white? black? piece-at filter-my-positions pos-on-board? pos-empty? piece)]))

(defn enemy-on-pos?
  "checks if an enemy piece is on the given position "
  [game-state position]
  (let [fig-at-pos (piece-at game-state position)] 
    (if (= :w (:turn game-state))
      (black? fig-at-pos)
      (white? fig-at-pos))))
                        
(defn steps-vertical [x c]
  (partition 2 (interleave (repeat x) c)))

(defn steps-down
  "downward steps that are possible from the given position - e.g. ((2 1) (2 0)) are the steps from (2, 2)"
  [[x y]]
  (steps-vertical x (range (dec y) -1 -1)))

(defn steps-up
  "upward steps that are possible from the given position - e.g. ((2 3) (2 4) (2 5) (2 6) (2 7)) are the steps from (2, 2)"
  [[x y]]
  (steps-vertical x (range (inc y) 8)))

(defn steps-horizontal
  [y c]
  (partition 2 (interleave c (repeat y))))

(defn steps-right
  "possible steps to the bright from (x, y)"
  [[x y]]
  (steps-horizontal y (range (inc x) 8)))

(defn steps-left
  "possible steps to the left from (x, y)"
  [[x y]]
  (steps-horizontal y (range (dec x) -1 -1)))

(defn steps-diagonal [fx fy position]
  "possible steps in a diagonal direction starting from (x, y)
   fx and fy are functions that are applied to x (and y, respectively) to determine the next step
   (steps-diagonal inc inc 0 0) returns ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))"
  
  (take-while #(apply pos-on-board? (partition 2 %))
              (drop 1 (iterate (fn [[a b]]
                                 (list (fx a) (fy b)))
                               position))))

(defn knight-steps
  "steps a knight could do from a given position - includes steps that are not on the game-state!"
  [[x y]]
  (map (fn [[xn yn]] (list (+ x xn) (+ y yn)))
       '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (-1 2) (1 -2) (-1 -2))))

(defn empty-moves [f game-state position]
  "moves on empty fields"
  (take-while (fn [pos] (pos-empty? game-state pos)) (f position)))

(defn fetch-direction [piece]
  "inverts the direction functions for black"
  (let [diag steps-diagonal]
    (if (white? piece)
      {:up steps-up
       :down steps-down
       :left steps-left
       :right steps-right
       :up-left (partial diag dec inc)
       :up-right (partial diag inc inc)
       :down-left (partial diag dec dec)
       :down-right (partial diag inc dec)}
      {:up steps-down
       :down steps-up
       :left steps-right
       :right steps-left
       :up-left (partial diag inc dec)
       :up-right (partial diag dec dec)
       :down-left (partial diag inc inc)
       :down-right (partial diag dec inc)})))

(defn steps-without-attack [game-state position dk n]
  "every step on an empty field.
   params: game-state, x y actual position, dk direction keyword, n number of allowed steps"
  (let [dir-fn (dk (fetch-direction (piece-at game-state position)))]
    (take n (empty-moves dir-fn game-state position))))

(defn steps-with-attack  [ game-state position dk n ]
  "every step on an enemy field, that isn't blocked by an own piece
   params: gamestate, x y actual position, dk direction-keyword, n number of allowed steps"
  (let [piece (piece-at game-state position)
        dir-fn (dk (fetch-direction piece))
        steps (take n (dir-fn position))
        enemy (first (filter (fn [[a b]] (enemy-on-pos? game-state [a b])) steps))]
    (when (every? (fn [pos] (= :_ (piece-at game-state pos))) (take-while #(not (= % enemy)) steps)) enemy)))

(defn all-steps
  "attacking and non-attacking steps"
  [game-state position dk n]
  (concat (steps-without-attack game-state position dk n) (steps-with-attack game-state position dk n)))

(defn get-moves "collects the moves into all posible directions"
  [game-state position dirs n]
  (let [steps (partial all-steps game-state position)]
    (partition 2 (flatten (map #(steps % n) dirs)))))

(def all-directions  '(:up-left :up-right :down-left :down-right :up :down :left :right))
(def infinite-steps 8)

(defmulti possible-moves piece)

(defmethod possible-moves :rook
  [game-state position]
    (get-moves game-state position '(:up :down :left :right) infinite-steps))

(defmethod possible-moves :pawn
  [game-state position]
  (let [[x y] position non-attacks (partial steps-without-attack game-state position) attacks (partial steps-with-attack game-state position)]
  (concat #{}
          (cond (and (white? (piece-at game-state position)) (= y 1)) (non-attacks :up 2)
                (and (black? (piece-at game-state position)) (= y 6)) (non-attacks :up 2)
                :else (non-attacks :up 1))
          (attacks :up-right 1) 
          (attacks :up-left  1))))

(defmethod possible-moves :queen
  [game-state position]
    (get-moves game-state position all-directions infinite-steps))

(defmethod possible-moves :king
  [game-state position]
    (get-moves game-state position all-directions 1))

(defmethod possible-moves :bishop
  [game-state position]
    (get-moves game-state position '(:up-left :up-right :down-left :down-right) infinite-steps))

(defmethod possible-moves :knight
  [game-state initial-position]
  (filter (fn [position] (and (pos-on-board? position) (let [piece (piece-at game-state position)] (or (= :_ piece) (enemy-on-pos? game-state position)))))
          (knight-steps initial-position)))

(defn fetch-positions [game-state]
  "returns all positions/coordinates that are occupied by pieces"
    (if (= :w (:turn game-state))
      (filter-my-positions white? game-state)
      (filter-my-positions black? game-state)))
    
(defn build-pair [k v]
  "k:starting position v:collection of target positions
   creates a sequence coordinate pairs for a turn '((x-old y-old) (x-new y-new))"
    (reduce #(conj %1 [k %2]) [] v))

(defn build-all-pairs [game-state positions]
  (map (fn [x] (let [moves (possible-moves game-state x)]
                 (when (seq (rest moves))
                   (build-pair x moves)))) positions))

(defn generate-moves [game-state]
  "generates all legal moves for the given game-state"
  (->> (filter #(not (nil? %))
     (build-all-pairs game-state (fetch-positions game-state)))
          flatten (partition 2) (partition 2)))