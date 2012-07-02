(ns chess.move-selection)

(defn select-move [game-state]
  (let [possible-moves (generator/gen-moves game-state)
        possible-states (apply-moves game-state possible-moves)
        rated-states (map rating/rate possible-states)]
    (choose-move possible-moves rated-states)))

(defn choose-move
  "finds the move that leads to the best rating"
  [moves rated-states]
  ...)