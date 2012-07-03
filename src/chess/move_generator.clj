(ns chess.move-generator
  (:use [chess.core :only (initial-board white? black? piece-at filter-my-positions move-piece pos-on-game-state? pos-empty? piece)]))

(defn enemy-on-pos?
  "checks if an enemy piece is on the given position "
  [game-state x y]
  (let [fig-at-pos (piece-at game-state x y)] 
    (if (= :w (:turn game-state))
      (black? fig-at-pos)
      (white? fig-at-pos))))
                        
(defn steps-vertical [x y c]
  (partition 2 (interleave (repeat x) c)))

(defn steps-down
  "downward steps that are possible from the given position - e.g. ((2 1) (2 0)) are the steps from (2, 2)"
  [x y]
  (steps-vertical x y (range (dec y) -1 -1)))

(defn steps-up
  "upward steps that are possible from the given position - e.g. ((2 3) (2 4) (2 5) (2 6) (2 7)) are the steps from (2, 2)"
  [x y]
  (steps-vertical x y (range (inc y) 8)))

(defn steps-horizontal
  [x y c]
  (partition 2 (interleave c (repeat y))))

(defn steps-right
  "possible steps to the right from (x, y)"
  [x y]
  (steps-horizontal x y (range (inc x) 8)))

(defn steps-left
  "possible steps to the left from (x, y)"
  [x y]
  (steps-horizontal x y (range (dec x) -1 -1)))

(defn steps-diagonal [fx fy x y]
  "possible steps in a diagonal direction starting from (x, y)
   fx and fy are functions that are applied to x (and y, respectively) to determine the next step

   (steps-diagonal inc inc 0 0) returns ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))"
  (take-while #(apply pos-on-game-state? %)
              (drop 1 (iterate (fn [[a b]]
                                 (list (fx a) (fy b)))
                               [x y]))))

(defn knight-steps
  "steps a knight could do from a given position - includes steps that are not on the game-state!"
  [x y]
  (map (fn [[xn yn]] (list (+ x xn) (+ y yn)))
       '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (-1 2) (1 -2) (-1 -2))))

(defn empty-moves [f game-state x y]
  (take-while (fn [[a b]] (pos-empty? game-state a b)) (f x y)))

(defn attacking-moves [f game-state x y]
  (first (drop-while (fn [[a b]] (pos-empty? game-state a b)) (f x y))))

(defn fetch-direction [piece]
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

(defn steps-without-attack [game-state x y k n]
  (let [dir-fn (k (fetch-direction (piece-at game-state x y)))]
    (take n (empty-moves dir-fn game-state x y))))

(defn steps-with-attack  [ game-state x y k n ]
  (let [ piece (piece-at game-state x y) dir-fn (k (fetch-direction piece)) steps (take n (dir-fn x y))
        enemy (first (filter (fn [[a b]] (enemy-on-pos? game-state a b)) steps))]
    (when (every? (fn [[a b]] (= :_ (piece-at game-state a b))) (take-while #(not (= % enemy)) steps)) enemy)))

(defn all-steps
  "attacking and non-attacking steps"
  [game-state x y k n]
  (concat (steps-without-attack game-state x y k n) (steps-with-attack game-state x y k n)))

(defn get-moves "collects the moves into all posible directions"
  [game-state [x y] dirs n]
  (let [steps (partial all-steps game-state x y)]
    (partition 2 (flatten (map #(steps % n) dirs)))))

(def all-directions  '(:up-left :up-right :down-left :down-right :up :down :left :right))
(def infinite-steps 8)

(defmulti possible-moves piece)

(defmethod possible-moves :rook
  [game-state position]
    (get-moves game-state position '(:up :down :left :right) infinite-steps))

(defmethod possible-moves :pawn
  [game-state position]
  (let [[x y] position non-attacks (partial steps-without-attack game-state x y) attacks (partial steps-with-attack game-state x y)]
  (concat #{}
          (cond (and (white? (piece-at game-state x y)) (= y 1)) (non-attacks :up 2)
                (and (black? (piece-at game-state x y)) (= y 6)) (non-attacks :up 2)
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
  [game-state position]
  (let [[x y] position]
  (filter (fn [[x y]] (and (pos-on-game-state? x y) (let [piece (piece-at game-state x y)] (or (= :_ piece) (enemy-on-pos? game-state x y)))))
          (knight-steps x y))))

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

