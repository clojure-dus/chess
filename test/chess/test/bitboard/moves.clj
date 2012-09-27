(ns chess.test.core
 (:use [chess.bitboard.chessboard])
  (:use [chess.bitboard.moves])
  (:use [clojure.test]))
(deftest test-find-first-one-bit
  (testing "some moves")

  (=[[[:R 0 1] [:R 0 2] [:R 0 3] [:R 0 4] [:R 0 5] [:R 0 6] [:R 0 7]]]
    (possible-moves (read-fen "8/8/8/8/8/8/8/R7 w KQkq - 0 1")))
  (=[[[:R 17 16] [:R 17 18] [:R 17 19]] [[:P 20 28]] [[:P 21 29]] [[:P 22 30]] [[:P 23 31]]]
    (possible-moves (read-fen "8/8/8/8/8/1R11PPPP/8/8 w KQkq - 0 1"))))
