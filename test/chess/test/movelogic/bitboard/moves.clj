(ns chess.test.movelogic.bitboard.moves
  (:use [chess.movelogic.bitboard moves chessboard])
  (:use [clojure.test]))

(defn- to-hashset[x] (apply hash-set x))

(deftest test-generate-moves
  (testing "some bitboard specific moves"
    (are [x y] (= x y)
         (to-hashset [[:R 0 56] [:R 0 48] [:R 0 40] [:R 0 32] [:R 0 24] [:R 0 16] [:R 0 8]
                      [:R 0 7] [:R 0 6] [:R 0 5] [:R 0 4] [:R 0 3] [:R 0 2] [:R 0 1]])
         (to-hashset (generate-moves (read-fen  "8/8/8/8/8/8/8/R7 w KQkq - 0 1")))
         
         (to-hashset [[:P 23 31] [:P 22 30] [:P 21 29] [:P 20 28] [:R 17 57] [:R 17 49] [:R 17 41]
                      [:R 17 33] [:R 17 25] [:R 17 19] [:R 17 18] [:R 17 16] [:R 17 9] [:R 17 1]])
         (to-hashset (generate-moves  (read-fen  "8/8/8/8/8/1R11PPPP/8/8 w KQkq - 0 1"))))))

(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
         [[:P 8 16][:P 8 24]]  (possible-moves initial-board (bit-set 0 8)))
    (testing "pawn is allowed to move 1 step up from any other position"
      (are [x y] (= x y)
           [[:P 16 24]] (possible-moves (move-piece initial-board :P 8 16) (bit-set 0 16))))
    (testing "no moves if pawn is blocked"
      (is (= [] (possible-moves (move-piece initial-board :P 48 16) (bit-set 0 8)))))
    (testing "pawn attacks diagonal"
      (is (= #{[:P 8 16][:P 8 24][:P 8 17]}
             (to-hashset (possible-moves (move-piece initial-board :p 49 17) (bit-set 0 8))))))))



(comment(run-tests))
