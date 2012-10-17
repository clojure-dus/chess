(ns chess.moves-impl 
  (:use [chess.core :only (white? black? piece-at filter-my-positions pos-on-board? pos-empty? piece move-piece)])
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
  (let [[x y] position non-attacks-fn (partial steps-without-attack game-state position) attacks-fn (partial steps-with-attack game-state position)
        is-white (white? (piece-at game-state position))]
    (let [non-attacking-moves (cond (and is-white (= y 1)) (non-attacks-fn steps-up 2)
                (and (not is-white) (= y 6)) (non-attacks-fn steps-down 2)
                :else (non-attacks-fn (pawn-non-attack-steps is-white) 1))]
      (filter #(not (nil? %))
          (concat non-attacking-moves
               (map #(attacks-fn % 1 non-attacking-moves) (pawn-attack-steps is-white)))))))

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


(defn enrich [game-state]
  (-> game-state (assoc :white-pos (filter-my-positions white? game-state))
                 (assoc :black-pos (filter-my-positions black? game-state))))


(defn generate-moves
  "generates all legal moves for the given game-state"
  [game-state]
  (let [enriched-game-state (enrich game-state)]
    (->> (filter #(not (nil? %))
                 (build-all-pairs enriched-game-state (fetch-positions enriched-game-state)))
         flatten (partition 2) (partition 2))))

(defn make-move
  "Attempts to move the piece from pos 'from' to pos 'to'.
   'from' and 'to' are x/y coordinates relative to lower left corner.
   Returns nil if the move is not allowed."
  [game-state from to]
  (when (some #{to} (possible-moves game-state from))
    (move-piece game-state from to)))