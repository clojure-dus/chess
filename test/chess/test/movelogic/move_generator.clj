(ns chess.test.movelogic.move_generator
  (:use [chess.movelogic.move-generator 
         :only ( print-board *move-engine* bitboard-engine vector-engine
                 initial-board move-piece possible-moves generate-moves)])
  (:use [clojure.test]))

(defn move-engine-fixture [f]
"execute each test first with vector and then with bitboard impl..."
  (binding [*move-engine* vector-engine]
            (f))
  (binding [*move-engine* bitboard-engine]
            (f)))

(use-fixtures :each move-engine-fixture)

(defn find-moves[game-state coord]
  (apply hash-set (possible-moves game-state coord)))

(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
         #{'(0 2) '(0 3)} (find-moves (initial-board) [0 1]) 
         #{'(0 5) '(0 4)} (find-moves (initial-board) [0 6])))
  (testing "pawn is allowed to move 1 step up from any other position"
    (are [x y] (= x y)
         #{'(0 3)} (find-moves (move-piece (initial-board) [0 1] [0 2]) [0 2])
         #{'(0 4)} (find-moves (move-piece (initial-board) [0 6] [0 5]) [0 5])))
  (testing "no moves if pawn is blocked"
    (is (= #{} (find-moves (move-piece (initial-board) [0 6] [0 2]) [0 1]))))
  (testing "pawn attacks diagonal"
    (is (= #{'(1 2) '(0 2) '(0 3)} (find-moves (move-piece (initial-board) [1 6] [1 2]) [0 1])))))

(deftest test-queen-moves
  (let [game-state (-> (initial-board) (move-piece [3 1] [4 4]) (move-piece [2 1] [2 2]))]
    (is (= #{'(2 1) '(1 2) '(0 3) '(3 1) '(3 2) '(3 3) '(3 4) '(3 5) '(3 6)} (find-moves game-state [3 0])))))

(deftest test-king-moves
  (let [game-state (-> (initial-board) (move-piece [4 1] [4 2]) (move-piece [5 1] [5 2]))]
    (is (= #{'(5 1) '(4 1)} (find-moves game-state [4 0])))))



(deftest test-rook-moves
  (let [game-state (-> (initial-board)  (move-piece [0 1] [0 6]) (move-piece [1 0] [1 6]))]
    (is (= #{'(0 1) '(0 2) '(0 3) '(0 4) '(0 5) '(1 0)} (find-moves game-state [0 0])))))

(deftest test-bishop-moves
  (let [game-state (-> (initial-board) (move-piece [1 1] [1 5]) (move-piece [3 1] [3 5])) ]
    (is (= #{'(1 1) '(0 2) '(3 1) '(4 2) '(5 3) '(6 4) '(7 5)} (find-moves game-state [2 0]))))
  (let [game-state (-> (initial-board) 
                         (assoc :turn :b) 
                          (move-piece [3 6] [1 1]) 
                            (move-piece [5 6] [1 2]))]
    (is (= #{'(3 6) '(5 6)} (find-moves game-state [4 7])))))

(deftest test-knight-moves
  (is (= #{'(2 2) '(0 2)} (find-moves (initial-board) [1 0]))))
(run-tests)
(deftest test-generate-moves
  (is (= #{[[0 1] [0 2]] 
         [[0 1] [0 3]] 
         [[1 0] [2 2]] 
         [[1 0] [0 2]] 
         [[1 1] [1 2]] 
         [[1 1] [1 3]] 
         [[2 1] [2 2]] 
         [[2 1] [2 3]] 
         [[3 1] [3 2]] 
         [[3 1] [3 3]] 
         [[4 1] [4 2]]  
         [[4 1] [4 3]] 
         [[5 1] [5 2]] 
         [[5 1] [5 3]] 
         [[6 0] [7 2]] 
         [[6 0] [5 2]] 
         [[6 1] [6 2]] [[6 1] [6 3]] [[7 1] [7 2]] [[7 1] [7 3]]}
            (apply hash-set (generate-moves (initial-board))))))
;(run-tests)
