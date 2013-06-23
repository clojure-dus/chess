(ns chess.movelogic.vector2d.moves-impl 
  (:require [clojure.core.reducers :as r])
  (:use [chess.movelogic.core :only (piece-map) ])
  (:use [chess.movelogic.vector2d.core :only (white? black?  piece-at filter-my-positions pos-on-board? pos-empty?  move-piece)])
  (:use [chess.movelogic.vector2d.directions]))

(defn- knight-steps
  "steps a knight could do from a given position - includes steps that are not on the game-state!"
  [[x y]]
  (r/map (fn [[xn yn]] [ (+ x xn) (+ y yn) ])
         '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (-1 2) (1 -2) (-1 -2))))


(defn- get-moves "collects the moves into all posible directions"
  [game-state position dirs n]
  (let [steps (partial all-steps game-state position)]
    (partition 2 (into [] (r/flatten (r/map #(steps % n) dirs))))))


(defn- build-pair [k v]
  "k:starting position v:collection of target positions
   creates a sequence coordinate pairs for a turn '((x-old y-old) (x-new y-new))"
  (when (seq v)
    (r/reduce #(conj (conj %1 k) %2) [] v)))

(declare possible-moves)

(defn fold-to-vec [coll]
  (r/fold (r/monoid into vector) conj coll))

(defn- build-all-pairs [game-state positions]
  (fold-to-vec (r/flatten (r/map (fn [x] (let [moves (possible-moves game-state x)]
                                           (build-pair x moves))) positions))))


(defn piece [game-state position]
  "returns a keyword (color-neutral) for the piece on the given position"
  (let [p (piece-at  game-state position)]
    (piece-map p) ))


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
      (into [] (r/filter #(not (nil? %))
                         (into non-attacking-moves (r/map #(attacks-fn % 1 non-attacking-moves) (pawn-attack-steps is-white))))))))

(defmethod possible-moves :queen
  [game-state position]
  (get-moves game-state position all-directions infinite-steps))

(defmethod possible-moves :king
  [game-state position]
  (get-moves game-state position all-directions 1))

(defmethod possible-moves :bishop 
  [game-state position]
  (get-moves game-state position [steps-up-left steps-up-right steps-down-left steps-down-right] infinite-steps))

(defmethod possible-moves :knight
  [game-state initial-position]
  (into [] (r/filter (fn [position] (and (pos-on-board? position) (let [piece (piece-at game-state position)] (or (= :_ piece) (enemy-on-pos? game-state position)))))
                     (knight-steps initial-position))))


(defn enrich [game-state]
  (-> game-state (assoc :white-pos (into [] (filter-my-positions white? game-state)))
      (assoc :black-pos (into [] (filter-my-positions black? game-state)))))

(defn generate-moves
  "generates all legal moves for the given game-state"
  [game-state]
  (let [enriched-game-state (enrich game-state)]
    (->> (filter #(not (nil? %))
                 (build-all-pairs enriched-game-state (fetch-positions enriched-game-state)))
         (partition 2) (partition 2))))

(defn make-move
  "Attempts to move the piece from pos 'from' to pos 'to'.
   'from' and 'to' are x/y coordinates relative to lower left corner.
   Returns nil if the move is not allowed."
  [game-state from to] 
  (when (and
         (or (and (= :w (:turn game-state)) (white? (piece-at game-state from)))
             (and (= :b (:turn game-state)) (black? (piece-at game-state from))))
         (some #{to} (possible-moves game-state from)))
(move-piece game-state from to)))