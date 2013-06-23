(ns chess.test.movelogic.core
  (:use [chess.core.core]
        [chess.movelogic.move-generator]
        [chess.movelogic.vector2d.core :only (pos-of-piece)]
        [clojure.test]))

(deftest test-piece-at
  (testing "piece-at should return the correct piece for the given game-state and position"
    (let [fut (partial get-piece (initial-board))]
      (are [x y] (= x y)
           :R (fut [0 0])
           :R (fut [7 0])
           :r (fut [0 7])
           :b (fut [2 7])
           :p (fut [0 6])
           :_ (fut [5 2])
           :P (fut [0 1])))))


(deftest test-piece-at-bitboard
  (testing "piece-at should return the correct piece for the given game-state and position"
    (binding [*move-engine* bitboard-engine]    
      (let [fut (partial get-piece (initial-board))]
        (are [x y] (= x y)
             :R (fut [0 0])
             :R (fut [7 0])
             :r (fut [0 7])
             :r (fut [7 7])
             :b (fut [2 7])
             :p (fut [0 6])
             :_ (fut [0 5])
             :_ (fut [5 2])
             :P (fut [0 1]))))))

(deftest test-move-piece
  (let [game-state (change-turn (initial-board))
        new-game-state (move-piece game-state [0 6] [0 5])]
    (are [x y] (= x y)
         :_ (get-piece new-game-state [0 6] )
         :p (get-piece new-game-state [0 5] ))
    (testing "preconditions of move-piece: source and target are on the game-state"
      (is (thrown? AssertionError (move-piece game-state [-1 0] [0 0])))
      (is (thrown? AssertionError (move-piece game-state [0 0] [0 8]))))))


(deftest test-move-piece-bitboard
  (binding [*move-engine* bitboard-engine]
    (let [game-state (change-turn (initial-board))
          new-game-state (move-piece game-state [0 6] [0 5])]
      (are [x y] (= x y)
           :_ (get-piece new-game-state [0 6] )
           :p (get-piece new-game-state [0 5] ))
      (testing "preconditions of move-piece bitboard: source and target are on the game-state"
        (is (thrown? AssertionError (move-piece game-state [-1 0] [0 0])))
        (is (thrown? AssertionError (move-piece game-state [0 0] [0 8])))))))


(deftest test-pos-of-piece
  (are [x y] (= x y)
       '((0 7) (7 7)) (pos-of-piece (initial-board) :r)
       '((0 0) (7 0)) (pos-of-piece (initial-board) :R)))
