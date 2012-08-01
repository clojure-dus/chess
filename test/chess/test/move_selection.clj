(ns chess.test.move-selection
  (:use [chess.core :only (initial-board move-piece read-fen)])
  (:use [chess.move-selection])
  (:use [clojure.test]))

(deftest test-min-max
  (is
    (= (list (list 7 4) (list 7 6))
    (min-max (read-fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1") 2))))