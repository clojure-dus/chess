(ns chess.movelogic.vector2d.moves-api
  (:require [chess.movelogic.protocol :as proto])
  (:require [chess.movelogic.vector2d.moves-impl :as impl])
  (:require [chess.movelogic.vector2d.core :as core])
  (:import ( chess.movelogic.vector2d.core GameState)))

(defn check? [game-state]
  "checks if the given board is in check"
  (let [opponent-moves (impl/generate-moves  (proto/change-turn game-state))
        king           (if (proto/whites-turn? game-state) :K :k)
        kings-pos      (core/pos-of-piece game-state king)]
    (true? (some (fn [[_ to]] (= king (core/piece-at game-state to))) opponent-moves))))


(extend-type GameState
 proto/MoveGenerator
  (generate-moves [this]
    (impl/generate-moves this))

  (possible-moves [this  coord]
    (impl/possible-moves this coord))

  (move-piece [this from to]
    (core/move-piece this from to))

  (test-check? [this]
    (check? this))

  (filter-positions-by-color [this  white]
    (core/filter-my-positions (if (= true white) core/white? core/black?) this))
  

  (get-piece [this  position] 
    (core/piece-at this position))

  (print-board [this]
    (core/print-board this)))

(def initial-board core/initial-board)

(defn read-fen [str]
    (core/read-fen str))