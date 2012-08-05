(ns chess.test.move-selection
  (:use [chess.core :only (initial-board move-piece read-fen)])
  (:use [chess.move-selection])
  (:use [clojure.test]))

(deftest test-min-max
  (is
    (= '((7 4) (7 6))
       (min-max (read-fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1") 2))))

(deftest test-check?
  (are [x y] (= x y)
       true (check? (read-fen "8/5k2/6P1/8/8/8/8/2K5 b - - 0 1"))
       true (check? (read-fen "8/5k2/4P3/8/8/8/8/2K5 b - - 0 1"))
       true (check? (read-fen "4k3/8/8/8/8/2p5/3K4/8 w - - 0 1"))
       true (check? (read-fen "4k3/8/8/8/8/4p3/3K4/8 w - - 0 1"))
       true (check? (read-fen "8/5k2/8/8/8/8/8/2K2Q2 b - - 0 1"))
       true (check? (read-fen "8/5k2/8/8/8/8/Q7/2K5 b - - 0 1"))
       false (check? (read-fen "8/5k2/8/8/2R5/8/Q7/2K5 w - - 0 1"))
       true (check? (read-fen "8/5k2/6B1/8/8/8/8/2K5 b - - 0 1"))
       true (check? (read-fen "8/5k2/8/8/8/8/B7/2K5 b - - 0 1"))
       false (check? (read-fen "8/5k2/8/8/2R5/8/B7/2K5 b - - 0 1"))))