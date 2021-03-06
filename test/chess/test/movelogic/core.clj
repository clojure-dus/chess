(ns chess.test.movelogic.core
  (:use [chess.core]
        [chess.movelogic.protocol :only (move-piece get-piece change-turn)]
        [chess.movelogic.vector2d.core :only (pos-of-piece)]
        [clojure.test]))

(defn vector-initial-board[]
  (initial-board {:move-logic :vector}))

(defn bitboard-initial-board[]
  (initial-board {:move-logic :bitboard}))

(deftest test-piece-at
  (testing "piece-at should return the correct piece for the given game-state and position"
    (let [fut (partial get-piece (vector-initial-board))]
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
    (let [fut (partial get-piece (bitboard-initial-board))]
      (are [x y] (= x y)
           :R (fut [0 0])
           :R (fut [7 0])
           :r (fut [0 7])
           :r (fut [7 7])
           :b (fut [2 7])
           :p (fut [0 6])
           :_ (fut [0 5])
           :_ (fut [5 2])
           :P (fut [0 1])))))

(deftest test-move-piece
  (let [game-state (change-turn (vector-initial-board))
        new-game-state (move-piece game-state [0 6] [0 5])]
    (are [x y] (= x y)
         :_ (get-piece new-game-state [0 6] )
         :p (get-piece new-game-state [0 5] ))))





(deftest test-move-piece-bitboard
  (let [game-state (change-turn (bitboard-initial-board))
        new-game-state (move-piece game-state [0 6] [0 5])]
    (are [x y] (= x y)
         :_ (get-piece new-game-state [0 6] )
         :p (get-piece new-game-state [0 5] ))
    (testing "preconditions of move-piece bitboard: source and target are on the game-state"
      (is (thrown? AssertionError (move-piece game-state [-1 0] [0 0])))
      (is (thrown? AssertionError (move-piece game-state [0 0] [0 8]))))))


(deftest test-pos-of-piece
  (are [x y] (= x y)
       '((0 7) (7 7)) (pos-of-piece (vector-initial-board) :r)
       '((0 0) (7 0)) (pos-of-piece (vector-initial-board) :R)))

(deftest test-read-fen
  (are [x y] (= x y)
       (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" {:move-logic :vector}) 
       (vector-initial-board)))

(deftest test-read-fen-bitboard
  (are [x y] (= x y)
       (:board (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") {:move-logic :bitboard}) 
       (:board (bitboard-initial-board))))
(run-tests)