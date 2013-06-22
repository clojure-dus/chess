(ns chess.movelogic.move-generator
  (:require [chess.movelogic.core :as core])
  (:require [chess.movelogic.vector2d.moves-api :only (move-generator) :as vector2d])
  (:require [chess.movelogic.bitboard.api       :only (move-generator) :as bitboard]))

(def  bitboard-engine  (bitboard/move-generator))

(def  vector-engine  (vector2d/move-generator))

(def ^:dynamic *move-engine* vector-engine)

(defn generate-moves[game-state]
  (core/generate-moves *move-engine* game-state))

(defn possible-moves [game-state coord]
  (core/possible-moves *move-engine* game-state coord))

(defn move-piece [game-state from to]
  (core/move-piece *move-engine* game-state from to))

(defn make-move [game-state from to]
  (core/make-move *move-engine* game-state from to))

(defn test-check? [game-state]
  (core/test-check? *move-engine* game-state))

(defn read-fen [str] 
  (core/read-fen *move-engine* str))

(defn move2board [[pos1 pos2] game-state]
  (core/move-piece *move-engine* game-state pos1 pos2))

(defn filter-positions-by-color [game-state white]
  (core/filter-positions-by-color *move-engine* game-state  white))

(defn initial-board[] (core/initial-board  *move-engine*))

(defn print-board[game-state]
 (core/print-board *move-engine* game-state))

(defn get-piece [game-state position]
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








