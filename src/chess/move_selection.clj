(ns chess.move-selection
  (:use [chess.move-generator :only (generate-moves)])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (move-piece)]))

(defn select-move  [game-state]
  (let [possible-moves  (generate-moves game-state)
        possible-states (map (fn [[pos1 pos2]] (move-piece game-state pos1 pos2)) possible-moves)
        states2moves    (zipmap possible-states possible-moves)
        ratedstates     (map rate possible-states)
        moves2rates     (zipmap possible-moves ratedstates)
        max-rate        (apply max ratedstates)]
        (first (first (filter #(= max-rate (val %)) moves2rates)))))
