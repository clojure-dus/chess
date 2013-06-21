(ns chess.movelogic.vector2d.directions
  (:use [chess.movelogic.vector2d.core :only (white? black? piece-at pos-on-board? pos-empty?)]))

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



(defn empty-moves [step-fn game-state position]
  "moves on empty fields"
  (take-while (fn [pos] (pos-empty? game-state pos)) (step-fn position)))

(def steps-up-left
  (partial steps-diagonal dec inc))

(def steps-up-right
  (partial steps-diagonal inc inc))

(def steps-down-left
  (partial steps-diagonal dec dec))

(def steps-down-right
  (partial steps-diagonal inc dec))

(def all-directions  [steps-up-left steps-up-right steps-down-left steps-down-right steps-up steps-down steps-left steps-right])

(def infinite-steps 8)


(defn steps-without-attack [game-state position dir-fn n]
  "every step on an empty field.
   params: game-state, position actual position, dir-fn direction function, n number of allowed steps"
    (take n (empty-moves dir-fn game-state position)))

(defn steps-with-attack  [ game-state position step-fn n non-attacking-steps]
  "every step on an enemy field, that isn't blocked by an own piece
   params: gamestate, position=actual position,dk direction function, n number of allowed steps"
  (let [piece (piece-at game-state position)
        steps (take n (step-fn position))
        non-attacks (set non-attacking-steps)
        enemy (first (filter (fn [[a b]] (enemy-on-pos? game-state [a b])) steps))]
    (when (every? #(contains? non-attacks %) (take-while #(not (= % enemy)) steps)) enemy)))


(defn all-steps
  "attacking and non-attacking steps"
  [game-state position dir-fn n]
  (let [non-attacking-steps (steps-without-attack game-state position dir-fn n)]
  (concat non-attacking-steps (steps-with-attack game-state position dir-fn n non-attacking-steps))))

(defn fetch-positions [game-state]
  "returns all positions/coordinates that are occupied by pieces"
    (if (= :w (:turn game-state))
      (:white-pos game-state)
      (:black-pos game-state)))
