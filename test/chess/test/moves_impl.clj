(ns chess.test.moves-impl
  (:use [chess.core :only (initial-board move-piece)])
  (:use [chess.moves-impl])
  (:use [clojure.test])
  (:use [chess.directions]))

(deftest test-steps-right
  (are [x y] (= x y)
       (list (list 2 3) (list 3 3) (list 4 3) (list 5 3) (list 6 3) (list 7 3)) (steps-right [1 3])))

(deftest test-steps-left
  (are [x y] (= x y)
       (list (list 1 3) (list 0 3)) (steps-left [2 3])))

(deftest test-steps-up
  (are [x y] (= x y)
       (list (list 2 4) (list 2 5) (list 2 6) (list 2 7)) (steps-up [2 3])))

(deftest test-steps-down
  (are [x y] (= x y)
       (list (list 2 2) (list 2 1) (list 2 0)) (steps-down [2 3])))

(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
       '((0 2) (0 3)) (possible-moves initial-board [0 1]) 
       '((0 5) (0 4)) (possible-moves initial-board [0 6])))
  (testing "pawn is allowed to move 1 step up from any other position"
    (are [x y] (= x y)
       '((0 3)) (possible-moves (move-piece initial-board [0 1] [0 2]) [0 2])
       '((0 4)) (possible-moves (move-piece initial-board [0 6] [0 5]) [0 5])))
  (testing "no moves if pawn is blocked"
    (is (= '() (possible-moves (move-piece initial-board [0 6] [0 2]) [0 1]))))
  (testing "pawn attacks diagonal"
    (is (= '((0 2) (0 3) (1 2)) (possible-moves (move-piece initial-board [1 6] [1 2]) [0 1])))))

(deftest test-queen-moves
  (let [game-state (-> initial-board (move-piece [3 1] [4 4]) (move-piece [2 1] [2 2]))]
    (is (= '((2 1) (1 2) (0 3) (3 1) (3 2) (3 3) (3 4) (3 5) (3 6)) (possible-moves game-state [3 0])))))

(deftest test-king-moves
  (let [game-state (-> initial-board (move-piece [4 1] [4 2]) (move-piece [5 1] [5 2]))]
    (is (= '((5 1) (4 1)) (possible-moves game-state [4 0])))))

(deftest test-rook-moves
  (let [game-state (-> initial-board  (move-piece [0 1] [0 6]) (move-piece [1 0] [1 6]))]
    (is (= '((0 1) (0 2) (0 3) (0 4) (0 5) (1 0)) (possible-moves game-state [0 0])))))

(deftest test-bishop-moves
  (let [game-state (-> initial-board (move-piece [1 1] [1 5]) (move-piece [3 1] [3 5])) ]
    (is (= '((1 1) (0 2) (3 1) (4 2) (5 3) (6 4) (7 5)) (possible-moves game-state [2 0]))))
  (let [game-state (-> initial-board (assoc :turn :b) (move-piece [3 6] [1 1]) (move-piece [5 6] [1 1]))]
    (is (= '((3 6) (5 6)) (possible-moves game-state [4 7])))))

(deftest test-knight-moves
  (is (= '((2 2) (0 2)) (possible-moves initial-board [1 0]))))

(deftest test-fetch-positions
  (is (= '((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 0) (3 1) (4 0) (4 1) (5 0) (5 1) (6 0) (6 1) (7 0) (7 1)) (fetch-positions (enrich initial-board))))
  (is (= '((0 6) (0 7) (1 6) (1 7) (2 6) (2 7) (3 6) (3 7) (4 6) (4 7) (5 6) (5 7) (6 6) (6 7) (7 6) (7 7)) (fetch-positions (assoc (enrich initial-board) :turn :b)))))

(deftest test-generate-moves
  (is (= '(((0 1) (0 2)) ((0 1) (0 3)) ((1 0) (2 2)) ((1 0) (0 2)) ((1 1) (1 2)) ((1 1) (1 3)) ((2 1) (2 2)) ((2 1) (2 3)) ((3 1) (3 2)) ((3 1) (3 3)) ((4 1) (4 2)) ((4 1) (4 3)) ((5 1) (5 2)) ((5 1) (5 3)) ((6 0) (7 2)) ((6 0) (5 2)) ((6 1) (6 2)) ((6 1) (6 3)) ((7 1) (7 2)) ((7 1) (7 3)))
        (generate-moves initial-board))))