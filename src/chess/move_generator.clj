(ns chess.move-generator
  (:use [chess.core :only (white? black? piece-at filter-my-positions pos-on-board? pos-empty? piece)])
  (:use [chess.directions]))

(defn- knight-steps
  "steps a knight could do from a given position - includes steps that are not on the game-state!"
  [[x y]]
  (map (fn [[xn yn]] (list (+ x xn) (+ y yn)))
       '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (-1 2) (1 -2) (-1 -2))))

(defn- get-moves "collects the moves into all posible directions"
  [game-state position dirs n]
  (let [steps (partial all-steps game-state position)]
    (partition 2 (flatten (map #(steps % n) dirs)))))


(defn- build-pair [k v]
  "k:starting position v:collection of target positions
   creates a sequence coordinate pairs for a turn '((x-old y-old) (x-new y-new))"
    (reduce #(conj %1 [k %2]) [] v))

(declare possible-moves)


(defn- build-all-pairs [game-state positions]
  (map (fn [x] (let [moves (possible-moves game-state x)]
                 (when (seq moves)
                   (build-pair x moves)))) positions))



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


(defn generate-moves [game-state]
  "generates all legal moves for the given game-state"
  (->> (filter #(not (nil? %))
     (build-all-pairs game-state (fetch-positions game-state)))
          flatten (partition 2) (partition 2)))