(ns chess.test.ai.rating.board-rating
  (:use [clojure.test] 
        [chess.core]
        [chess.ai.rating.board-rating]))

(deftest test-color-rating
  (is (= 39 (color-rating  (initial-board) true)))
  (is (= 39 (color-rating  (initial-board) false))))

(deftest test-rate
  (is (= 0 (rate (initial-board)))))

