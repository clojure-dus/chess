(ns chess.moves-api
  (:require [chess.moves-impl :as impl]))


(defn generate-moves [game-state]
  (impl/generate-moves game-state))