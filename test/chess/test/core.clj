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


(deftest test-moves-right
  (are [x y] (= x y)
       (list (list 2 3) (list 3 3) (list 4 3) (list 5 3) (list 6 3) (list 7 3)) (moves-right 1 3)))

(deftest test-moves-left
  (are [x y] (= x y)
       (list (list 1 3) (list 0 3)) (moves-left 2 3)))

(deftest test-moves-up
  (are [x y] (= x y)
       (list (list 2 4) (list 2 5) (list 2 6) (list 2 7)) (moves-up 2 3)))

(deftest test-moves-down
  (are [x y] (= x y)
       (list (list 2 2) (list 2 1) (list 2 0)) (moves-down 2 3)))