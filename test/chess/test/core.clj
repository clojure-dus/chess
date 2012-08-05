(ns chess.test.core
  (:use [chess.core])
  (:use [clojure.test]))

(deftest test-piece-at
  (testing "piece-at should return the correct piece for the given game-state and position"
    (let [fut (partial piece-at initial-board)]
    (are [x y] (= x y)
     :R (fut [0 0])
     :r (fut [0 7])
     :b (fut [2 7])
     :p (fut [0 6])
     :_ (fut [5 2])
     :P (fut [0 1])))))

(deftest test-move-piece
  (let [new-game-state (move-piece initial-board [0 6] [0 5])]
    (are [x y] (= x y)
         :_ (piece-at new-game-state [0 6] )
         :p (piece-at new-game-state [0 5] ))
    (testing "preconditions of move-piece: source and target are on the game-state"
    (is (thrown? AssertionError (move-piece initial-board [-1 0] [0 0])))
    (is (thrown? AssertionError (move-piece initial-board [0 0] [0 8]))))))


(deftest test-read-fen
  (are [x y] (= x y)
       (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") initial-board))

(deftest test-pos-of-piece
  (are [x y] (= x y)
       '((0 7) (7 7)) (pos-of-piece initial-board :r)
       '((0 0) (7 0)) (pos-of-piece initial-board :R)))