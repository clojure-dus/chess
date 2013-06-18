 (ns chess.movelogic.move-generator
  (:require [chess.movelogic.vector2d.moves-api    :only  (move-generator):as default])
  (:require [chess.movelogic.bitboard.api :only (move-generator) :as bitboard]))
 
  (defprotocol MoveGenerator
     (generate-moves [this game-state])
     (move-piece [this game-state from to])
     (test-check? [this game-state])
     (read-fen [this str]))


  (def ^:dynamic *move-engine*  (default/move-generator))

  (defn move2board [[pos1 pos2] game-state]
     (move-piece *move-engine* game-state pos1 pos2))

  (defn generate-moves[game-state]
    (generate-moves *move-engine* game-state))
  (defn test-check? [game-state]
  	(test-check? *move-engine* game-state))

(defn whites-turn? [game-state]
  (= :w (:turn game-state)))


(defn change-turn
  "changes the turn to the next player"
  [game-state]
  (if (whites-turn? game-state)
    (assoc game-state :turn :b)
    (assoc game-state :turn :w)))




