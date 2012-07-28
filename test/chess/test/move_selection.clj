(ns chess.test.move-selection
  (:use [chess.core :only (initial-board move-piece read-fen)])
  (:use [chess.move-selection])
  (:use [clojure.test]))

(deftest test-select-move
   (let [board (move-piece initial-board [4 1] [4 6])]
    (is
     (list (list 4 6) (list 3 7)) (select-move board))))

(comment (deftest test-min-max
  (is
    (list (7 4) (7 6))
    (min-max (read-fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1") 2))))