(ns chess.core.core
  (:require [chess.movelogic.vector2d.moves-api :only (move-generator) :as vector2d])
  (:require [chess.movelogic.bitboard.api       :only (move-generator) :as bitboard]))


(def  bitboard-engine  (bitboard/move-generator))

(def  vector-engine  (vector2d/move-generator))

(def ^:dynamic *move-engine* vector-engine)

