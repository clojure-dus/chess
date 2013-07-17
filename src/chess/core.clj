(ns chess.core
  (:require [chess.movelogic.bitboard.api :as bitboard :only [initial-board read-fen]])  
  (:require [chess.movelogic.vector2d.moves-api :as vector :only [initial-board read-fen]])  
  (:require [chess.movelogic.protocol  :as move-logic])
  (:require [chess.ai.move-selection.min-max :as min-max]))

(defn choose-movelogic [& config]
  (if config (apply :move-logic config)
      :default))

(defmulti initial-board choose-movelogic)

(defmethod initial-board :default []
  vector/initial-board)

(defmethod initial-board :bitboard [config] 
  (merge bitboard/initial-board config))

(defmethod initial-board :vector [config]
  (merge  vector/initial-board config))

(defmulti read-fen (fn [str & config]
                     (if config 
                       (apply :move-logic config) :default)))

(defmethod read-fen :default [str]
  (bitboard/read-fen str))

(defmethod read-fen :bitboard [str config]
  (merge (bitboard/read-fen str) config))

(defmethod read-fen :vector [str config]
  (merge (vector/read-fen str) config))

(defmulti select-move (fn [game-state] (:ai game-state)))

(defmethod select-move :min-max [game-state]
  (min-max/select-move game-state (or (:min-max-depth game-state) 2)))

(defmethod select-move :default [game-state]
  (min-max/select-move game-state (or (:min-max-depth game-state) 2)))

(defn generate-moves [game-state] 
  (move-logic/generate-moves game-state))

(defn possible-moves [game-state pos] 
  (move-logic/possible-moves game-state pos))

(comment (select-move (initial-board {:move-logic :bitboard :ai :min-max :min-max-depth 5}))
         (generate-moves (initial-board {:move-logic :vector}))
         (choose-movelogic {:move-logic :bitboard}))

