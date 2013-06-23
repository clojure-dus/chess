(ns chess.test.movelogic.fen
  (:use clojure.test
        [chess.core.core]
        [chess.movelogic.move-generator]))

(deftest test-read-fen
  (are [x y] (= x y)
       (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") 
       (initial-board)))

(deftest test-read-fen-bitboard
  (binding [*move-engine* bitboard-engine] 
    (are [x y] (= x y)
         (:board (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")) 
         (:board (initial-board)))))


