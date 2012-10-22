(ns chess.moves-api
  (:require [chess.moves-impl :as impl]))

(defn generate-moves [game-state]
  (impl/generate-moves game-state))

(defn make-move
  "Attempts to move the piece from pos 'from' to pos 'to'.
   'from' and 'to' are x/y coordinates relative to lower left corner.
   Returns nil if the move is not allowed."
  [game-state from to]
  (impl/make-move game-state from to))