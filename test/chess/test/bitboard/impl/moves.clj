(ns chess.test.bitboard.impl.moves
  (:use [chess.bitboard.impl.chessboard :as chess])
  (:require [chess.bitboard.impl.moves :as moves])
  (:require [chess.bitboard.api :as api])
  (:use [clojure.test]))
(deftest test-generate-moves
  (testing "some moves")

  (=[[:R 0 56] [:R 0 48] [:R 0 40] [:R 0 32] [:R 0 24] [:R 0 16] [:R 0 8]
     [:R 0 7] [:R 0 6] [:R 0 5] [:R 0 4] [:R 0 3] [:R 0 2] [:R 0 1]]
     (moves/generate-moves (api/read-fen "8/8/8/8/8/8/8/R7 w KQkq - 0 1")))
  (=[[:P 23 31] [:P 22 30] [:P 21 29] [:P 20 28] [:R 17 57] [:R 17 49] [:R 17 41]
     [:R 17 33] [:R 17 25] [:R 17 19] [:R 17 18] [:R 17 16] [:R 17 9] [:R 17 1]]
     (moves/generate-moves (api/read-fen "8/8/8/8/8/1R11PPPP/8/8 w KQkq - 0 1"))))



(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
         #{'(0 2) '(0 3)}  (apply hash-set (api/possible-moves api/initial-board [0 1]))
         #{'(0 5) '(0 4)}  (apply hash-set (api/possible-moves api/initial-board [0 6]))))
  (testing "pawn is allowed to move 1 step up from any other position"
    (are [x y] (= x y)
         '((0 3)) (api/possible-moves (api/move-piece api/initial-board [0 1] [0 2]) [0 2])
         '((0 4)) (api/possible-moves (api/move-piece api/initial-board [0 6] [0 5]) [0 5])))
  (testing "no moves if pawn is blocked"
    (is (= '() (api/possible-moves (api/move-piece api/initial-board [0 6] [0 2]) [0 1]))))
  (testing "pawn attacks diagonal"
    (is (= #{'(0 2) '(0 3) '(1 2)}
           (apply hash-set (api/possible-moves
                            (api/move-piece api/initial-board [1 6] [1 2]) [0 1]))))))
(comment (run-tests))
