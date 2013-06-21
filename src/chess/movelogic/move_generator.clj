(ns chess.movelogic.move-generator
  (:require [chess.movelogic.core :as core])
  (:require [chess.movelogic.vector2d.moves-api :only (move-generator) :as default])
                                        ;  (:require [chess.movelogic.bitboard.api       :only (move-generator) :as bitboard])
)


(def ^:dynamic *move-engine*  (default/move-generator))

(defn generate-moves[game-state]
  (core/generate-moves *move-engine* game-state))

(defn move-piece [this game-state from to]
  (core/move-piece *move-engine* game-state from to))

(defn test-check? [game-state]
  (core/test-check? *move-engine* game-state))

(defn read-fen [str] 
  (core/read-fen *move-engine* str))

(defn move2board [[pos1 pos2] game-state]
  (core/move-piece *move-engine* game-state pos1 pos2))

(defn filter-positions-by-color [game-state color-fn]
  (core/filter-positions-by-color *move-engine* game-state  color-fn))

(defn get-piece [this game-state position]
  (core/get-piece *move-engine* game-state position))

(defn whites-turn? [game-state]
  (core/whites-turn? game-state))


(defn piece [game-state position]
  "returns a keyword (color-neutral) for the piece on the given position"
  (let [p (get-piece *move-engine* game-state position)]
    (p core/piece-map)))


(defn change-turn
  "changes the turn to the next player"
  [game-state]
  (core/change-turn game-state))




