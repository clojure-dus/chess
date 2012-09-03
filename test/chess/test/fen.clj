(ns chess.test.fen
  (:use clojure.test
        chess.fen
        [chess.core :only [initial-board]]))

(deftest test-read-fen
  (are [x y] (= x y)
       (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") initial-board))