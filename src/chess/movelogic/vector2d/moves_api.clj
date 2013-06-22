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

    (possible-moves [this game-state coord]
      (impl/possible-moves game-state coord))

    (move-piece [this game-state from to]
      (core/move-piece game-state from to))

    (make-move [this game-state from to]
      (impl/make-move game-state from to))

    (test-check? [this game-state]
      (check? game-state))

    (read-fen [this str]
      (read-fen str))

    (filter-positions-by-color [this game-state white]
      (core/filter-my-positions (if (= true white) core/white? core/black?) game-state))
    
    (initial-board [this]
      core/initial-board)

    (get-piece [this game-state position] 
      (core/piece-at game-state position))

    (print-board [this game-state]
      (core/print-board game-state))))
