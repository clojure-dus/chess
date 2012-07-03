(ns chess.board-rating
  (:use [chess.core :only (initial-board white? black? filter-my-positions)]))

(defn rate [game-state]
  (filter-my-positions white? game-state))