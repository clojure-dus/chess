 (ns chess.move-generator)
  (defprotocol MoveGenerator
     (generate-moves [this game-state])
     (move-piece [this game-state from to])
     (test-check? [this game-state])
     (read-fen [this str]))
