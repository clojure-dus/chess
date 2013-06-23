(ns chess.test.movelogic.move_generator
  (:use [chess.core.core])
  (:use [chess.movelogic.move-generator 
         :only ( print-board  read-fen
                 initial-board move-piece possible-moves generate-moves test-check?)])
  (:use [clojure.test]))

                                        ;
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
                                        ;(run-tests)
                                        ;(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))

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


(deftest test-possible-moves
  (is (= #{'(0 2) '(2 2)} (find-moves (initial-board) [1 0]))))


(deftest test-api-check?
  (are [x y] (= x y)
       true (test-check?  (read-fen  "8/5k2/6P1/8/8/8/8/2K5 b - - 0 1"))
       true (test-check?  (read-fen  "8/5k2/4P3/8/8/8/8/2K5 b - - 0 1"))
       true (test-check?  (read-fen  "4k3/8/8/8/8/2p5/3K4/8 w - - 0 1"))
       true (test-check?  (read-fen  "4k3/8/8/8/8/4p3/3K4/8 w - - 0 1"))
       true (test-check?  (read-fen  "8/5k2/8/8/8/8/8/2K2Q2 b - - 0 1"))
       true (test-check?  (read-fen  "8/5k2/8/8/8/8/Q7/2K5 b - - 0 1"))
       false(test-check?  (read-fen  "8/5k2/8/8/2R5/8/Q7/2K5 w - - 0 1"))
       true (test-check?  (read-fen  "8/5k2/6B1/8/8/8/8/2K5 b - - 0 1"))
       true (test-check?  (read-fen  "8/5k2/8/8/8/8/B7/2K5 b - - 0 1"))
       false (test-check? (read-fen  "8/5k2/8/8/2R5/8/B7/2K5 b - - 0 1"))))


(deftest test-rochade
  (testing "some rochade (with attacks)  only bitboard implementation")
  (binding [*move-engine* bitboard-engine]

    (let [game-state (-> (initial-board)
                         (move-piece [4 1] [4 3])
                         (move-piece [5 0] [4 1])
                         (move-piece [6 0] [5 2])
                         (move-piece [3 0] [3 2])
                         (move-piece [2 0] [3 1])
                         (move-piece [2 1] [2 2])
                         (move-piece [3 0] [2 1])
                         (move-piece [1 0] [0 2]))]
      (is (= #{[6 0] [5 0] [3 0] [2 0]}
             (find-moves game-state [4 0]))))

    (let [game-state (-> (initial-board)
                         (move-piece [6 6] [6 5])
                         (move-piece [5 7] [6 6])
                         (move-piece [6 7] [5 5])
                         (move-piece [4 6] [4 5])
                         (move-piece [3 7] [4 6])
                         (move-piece [3 6] [3 5])
                         (move-piece [2 7] [3 6])
                         (move-piece [1 7] [2 5]))
          game-state (assoc game-state :turn :b)]
      (is (= #{[2 7] [3 7] [5 7] [6 7]}
             (find-moves  game-state [4 7])))
      (is (= #{[3 7] [5 7]}
             (find-moves (read-fen "r3k2r/pppppppp/1N5N/8/8/8/8/8 b KQkq - 0 1") [4 7])))
      (is (= #{[5 6] [2 7] [3 7] [5 7]}
             (find-moves (read-fen "r3k2r/ppppp1pp/8/8/5R2/8/8/8 b KQkq - 0 1") [4 7]))))))
