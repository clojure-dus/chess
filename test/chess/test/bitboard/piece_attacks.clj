(ns chess.test.core
 (:use [chess.bitboard.chessboard])
 (:use [chess.bitboard.piece-attacks])
 (:use [chess.bitboard.chessboard])
 (:use [chess.bitboard.bitoperations])
  (:use [clojure.test]))

(deftest test-knight-attack-array
    (testing "testing knight attack array"
      (is (= 132096            (aget knight-attack-array 0)))
      (is (= 567348067172352   (aget knight-attack-array 32)))
      (is (= 9077567998918656  (aget knight-attack-array 63)))))


(deftest test-pawn-white-move-array
    (testing "testing pawn move array"
      (is (= 67371008        (aget pawn-white-move-array 10)))
      (is (= 16777216        (aget pawn-white-move-array 16)))))
(run-tests)
