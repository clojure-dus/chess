(ns chess.test.core
  (:use [chess.core])
  (:use [clojure.test]))

(deftest test-figure-at
  (testing "figure-at should return the correct figure for the given board and position"
    (let [fut (partial figure-at initial-board)]
    (are [x y] (= x y)
     :R (fut 0 0)
     :r (fut 0 7)
     :b (fut 2 7)
     :p (fut 0 6)
     :_ (fut 5 2)
     :P (fut 0 1)))))

(deftest test-move-figure
  (let [new-board (move-figure initial-board 0 6 0 5)]
    (are [x y] (= x y)
         :_ (figure-at new-board 0 6 )
         :p (figure-at new-board 0 5 ))
    (testing "preconditions of move-figure: source and target are on the board, source is not empty"
    (is (thrown? AssertionError (move-figure initial-board 0 3 0 4)))
    (is (thrown? AssertionError (move-figure initial-board -1 0 0 0)))
    (is (thrown? AssertionError (move-figure initial-board 0 0 0 8))))))

(deftest test-steps-right
  (are [x y] (= x y)
       (list (list 2 3) (list 3 3) (list 4 3) (list 5 3) (list 6 3) (list 7 3)) (steps-right 1 3)))

(deftest test-steps-left
  (are [x y] (= x y)
       (list (list 1 3) (list 0 3)) (steps-left 2 3)))

(deftest test-steps-up
  (are [x y] (= x y)
       (list (list 2 4) (list 2 5) (list 2 6) (list 2 7)) (steps-up 2 3)))

(deftest test-steps-down
  (are [x y] (= x y)
       (list (list 2 2) (list 2 1) (list 2 0)) (steps-down 2 3)))

(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
       (list (list 0 2) (list 0 3)) (pawn-moves initial-board 0 1) 
       (list (list 0 5) (list 0 4)) (pawn-moves initial-board 0 6)))
  (testing "pawn is allowed to move 1 step up from any other position"
    (are [x y] (= x y)
       (list (list 0 3)) (pawn-moves (move-figure initial-board 0 1 0 2) 0 2)
       (list (list 0 4)) (pawn-moves (move-figure initial-board 0 6 0 5) 0 5)))
  (testing "no moves if pawn is blocked"
    (is (list) (pawn-moves (move-figure initial-board 0 6 0 2) 0 1)))
  (testing "pawn attacks diagonal"
    (is (list (list 0 2) (list 0 3) (list 1 2)) (pawn-moves (move-figure initial-board 1 6 1 2) 0 1))))

(deftest test-queen-moves
  (let [board (-> initial-board (move-figure 3 1 4 4) (move-figure 2 1 2 2))]
    (is '((2 1) (1 2) (0 3) (3 1) (3 2) (3 3) (3 4) (3 5) (3 6)) (queen-moves board 3 0))))

(deftest test-king-moves
  (let [board (-> initial-board (move-figure 4 1 4 2) (move-figure 5 1 5 2))]
    (is '((4 1) (5 1)) (king-moves board 4 0))))

(deftest test-rook-moves
  (let [board (-> initial-board  (move-figure 0 1 0 6) (move-figure 1 0 1 6))]
    (is '((0 1) (0 2) (0 3) (0 4) (0 5) (1 0)) (rook-moves board 0 0))))

(deftest test-bishop-moves
  (let [board (-> initial-board (move-figure 1 1 1 5) (move-figure 3 1 3 5)) ]
    (is '((1 1) (0 2) (3 1) (4 2) (5 3) (6 4) (7 5)) (bishop-moves board 2 0))))