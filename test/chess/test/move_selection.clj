(ns chess.test.move-selection
  (:use [chess.core :only (initial-board move-piece)])
  (:use [chess.move-selection])
  (:use [clojure.test]))

(deftest test-select-move
   (let [board (move-piece initial-board [4 1] [4 6])]
    (is
      '((4 6) (3 7)) (select-move board))))