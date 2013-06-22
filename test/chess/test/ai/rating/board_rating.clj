(ns chess.test.ai.rating.board-rating
  (:use [clojure.test] 
        [chess.ai.rating.board-rating] 
        [chess.movelogic.fen] 
        [chess.movelogic.move-generator :only (initial-board *move-engine* bitboard-engine vector-engine)]))

(deftest test-color-rating
  (is (= 39 (color-rating  (initial-board) true)))
  (is (= 39 (color-rating  (initial-board) false))))

(deftest test-color-rating-bitboard
  (binding [*move-engine* bitboard-engine ]
    (is (= 39 (color-rating (initial-board) true)))
    (is (= 39 (color-rating (initial-board) false)))))

(deftest test-rate
  (is (= 0 (rate (initial-board)))))

