(ns chess.moves-api
   (:use [chess.fen])
  (:require [chess.move-generator :as gen])
  (:require [chess.moves-impl :as impl])
  (:require [chess.core :as core])
  )

(defn generate-moves [game-state]
  (impl/generate-moves game-state))

(defn make-move
  "Attempts to move the piece from pos 'from' to pos 'to'.
   'from' and 'to' are x/y coordinates relative to lower left corner.
   Returns nil if the move is not allowed."
  [game-state from to]
  (impl/make-move game-state from to))

(defn check? [game-state]
  "checks if the given board is in check"
  (let [opponent-moves (generate-moves  (core/change-turn game-state))
        king           (if (core/whites-turn? game-state) :K :k)
        kings-pos      (core/pos-of-piece game-state king)]
     (true? (some (fn [[_ to]] (= king (core/piece-at game-state to))) opponent-moves))))



(defn move-generator []
  (reify gen/MoveGenerator
    (generate-moves [this game-state]
      (generate-moves game-state))

    (move-piece [this game-state from to]
      (make-move game-state from to))

    (test-check? [this game-state]
       (check? game-state))
   (get-piece [this game-state position] (piece-at game-state position))
    (read-fen [this str]
       (read-fen str))))
