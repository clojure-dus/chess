(ns chess.test.core
(:use [chess.core])
(:use [clojure.test]))


(defn vector-initial-board[]
  (initial-board {:move-logic :vector :ai :min-max :min-max-depth 2}))

(defn bitboard-initial-board[] 
  (initial-board {:move-logic :bitboard :ai :min-max :min-max-depth 2}))


(deftest test-select-move
  (is (select-move  (vector-initial-board)) (select-move (bitboard-initial-board))))

