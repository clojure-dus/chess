(ns chess.ai.rating.board-rating
  (:require [clojure.core.reducers :as r])
  (:use [chess.movelogic.move-generator :only (initial-board  filter-positions-by-color piece)]))

(defn rate-position [game-state position]
  (get {:pawn 1 :knight 3 :bishop 3 :rook 5 :queen 9} (piece game-state position) 0))

(defn color-rating [game-state for-white]
  (r/reduce #(+ %1 (rate-position game-state %2)) 0 (filter-positions-by-color game-state for-white)))

(defn rate [game-state]
  (let [rating-white (color-rating game-state true)
        rating-black (color-rating game-state false)]
    (if (= :w (:turn game-state))
      (- rating-white rating-black)
      (- rating-black rating-white))))
