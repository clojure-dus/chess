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
  "x: constant value on x-axis
   c: collection of values on the y-axis"
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
  "y: constant value on y-axis
   c: collection of values on the x-axis"
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

(def steps-up-left
  (partial steps-diagonal dec inc))

(def steps-up-right
  (partial steps-diagonal inc inc))

(def steps-down-left
  (partial steps-diagonal dec dec))

(def steps-down-right
  (partial steps-diagonal inc dec))

(defn steps-without-attack [game-state position dir-fn n]
  "every step on an empty field.
   params: game-state, position actual position, dir-fn direction function, n number of allowed steps"
    (take n (empty-moves dir-fn game-state position)))

(defn steps-with-attack  [ game-state position dir-fn n ]
  "every step on an enemy field, that isn't blocked by an own piece
   params: gamestate, position=actual position,dk direction function, n number of allowed steps"
  (let [piece (piece-at game-state position)
        steps (take n (dir-fn position))
        enemy (first (filter (fn [[a b]] (enemy-on-pos? game-state [a b])) steps))]
    (when (every? (fn [pos] (= :_ (piece-at game-state pos))) (take-while #(not (= % enemy)) steps)) enemy)))

(defn all-steps
  "attacking and non-attacking steps"
  [game-state position dir-fn n]
  (concat (steps-without-attack game-state position dir-fn n) (steps-with-attack game-state position dir-fn n)))

(defn get-moves "collects the moves into all posible directions"
  [game-state position dirs n]
  (let [steps (partial all-steps game-state position)]
    (partition 2 (flatten (map #(steps % n) dirs)))))

(def all-directions  [steps-up-left steps-up-right steps-down-left steps-down-right steps-up steps-down steps-left steps-right])

(def infinite-steps 8)

(defmulti possible-moves piece)

(defmethod possible-moves :rook
  [game-state position]
  (get-moves game-state position [steps-up steps-down steps-left steps-right] infinite-steps))

(defn pawn-attack-steps [white]
  (if white
    [steps-up-left steps-up-right]
    [steps-down-right steps-down-left]))

(defn pawn-non-attack-steps [white]
  (if white
    steps-up
    steps-down))

(defmethod possible-moves :pawn
  [game-state position]
  (let [[x y] position non-attacks (partial steps-without-attack game-state position) attacks (partial steps-with-attack game-state position)
        white (white? (piece-at game-state position))]
    (filter #(not (nil? %))
            (concat
             (cond (and white (= y 1)) (non-attacks steps-up 2)
                (and (not white) (= y 6)) (non-attacks steps-down 2)
                :else (non-attacks (pawn-non-attack-steps white) 1))
                (map #(attacks % 1) (pawn-attack-steps white))))))

(defmethod possible-moves :queen
  [game-state position]
    (get-moves game-state position all-directions infinite-steps))

(defmethod possible-moves :king
  [game-state position]
    (get-moves game-state position all-directions 1))

(defmethod possible-moves :bishop
  [game-state position]
    (get-moves game-state position (list steps-up-left steps-up-right steps-down-left steps-down-right) infinite-steps))

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
                 (when (seq moves)
                   (build-pair x moves)))) positions))

(defn generate-moves [game-state]
  "generates all legal moves for the given game-state"
  (->> (filter #(not (nil? %))
     (build-all-pairs game-state (fetch-positions game-state)))
          flatten (partition 2) (partition 2)))