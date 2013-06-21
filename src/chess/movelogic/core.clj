(ns chess.movelogic.core)

(defprotocol MoveGenerator
  (generate-moves [this game-state])
  (move-piece [this game-state from to])
  (test-check? [this game-state])
  (read-fen [this str])
  (filter-positions-by-color [this game-state white])   
  (initial-board [this])
  (get-piece [this game-state position]))

(def piece-map
  {:r :rook, :R :rook,
   :p :pawn, :P :pawn,
   :n :knight,:N :knight,
   :b :bishop,:B :bishop,
   :q :queen, :Q :queen,
   :k :king,  :K :king})

(defn whites-turn? [game-state]
  (= :w (:turn game-state)))

(defn change-turn
  "changes the turn to the next player"
  [game-state]
  (if (whites-turn? game-state)
    (assoc game-state :turn :b)
    (assoc game-state :turn :w)))
