(ns chess.test.board-rating
  (:use [clojure.test] [chess.board-rating] [chess.core :only (initial-board white? black?)]))

(deftest test-color-rating
  (is (= 39 (color-rating initial-board white?)))
  (is (= 39 (color-rating initial-board black?))))

(deftest test-rate
  (is (= 0 (rate initial-board))))