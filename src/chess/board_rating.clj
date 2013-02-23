(ns chess.board-rating
  (:require [clojure.core.reducers :as r])
  (:use [chess.core :only (initial-board white? black? filter-my-positions piece)]))

(defn rate-position [game-state position]
  (get {:pawn 1 :knight 3 :bishop 3 :rook 5 :queen 9} (piece game-state position) 0))

(defn color-rating [game-state color-fn]
  (r/reduce #(+ %1 (rate-position game-state %2)) 0 (filter-my-positions color-fn game-state)))

(defn rate [game-state]
  (let [rating-white (color-rating game-state white?)
        rating-black (color-rating game-state black?)]
    (if (= :w (:turn game-state))
      (- rating-white rating-black)
      (- rating-black rating-white))))
