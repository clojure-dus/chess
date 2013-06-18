(defn select-move
  "Generates the next move for the given game-state.
   Returns seq with two positions 'from' and 'two'."
  [game-state]
  (min-max game-state 2))