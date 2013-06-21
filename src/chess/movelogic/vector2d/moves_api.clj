(ns chess.movelogic.vector2d.moves-api
  (:use     [chess.movelogic.fen :only (read-fen)])
  (:require [chess.movelogic.core :as gen])
  (:require [chess.movelogic.vector2d.moves-impl :as impl])
  (:require [chess.movelogic.vector2d.core :as core]))

(defn check? [game-state]
  "checks if the given board is in check"
  (let [opponent-moves (impl/generate-moves  (gen/change-turn game-state))
        king           (if (gen/whites-turn? game-state) :K :k)
        kings-pos      (core/pos-of-piece game-state king)]
     (true? (some (fn [[_ to]] (= king (core/piece-at game-state to))) opponent-moves))))



(defn move-generator []
  (reify gen/MoveGenerator
    (generate-moves [this game-state]
      (impl/generate-moves game-state))

    (move-piece [this game-state from to]
      (impl/make-move game-state from to))

    (test-check? [this game-state]
      (check? game-state))

    (read-fen [this str]
      (read-fen str))

    (filter-positions-by-color [this game-state color-fn]
      (core/filter-my-positions color-fn game-state))

    (get-piece [this game-state position] 
      (core/piece-at game-state position))))
