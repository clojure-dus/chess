(ns chess.test.bitboard.impl.moves
  (:use [chess.bitboard.impl.chessboard :as chess])
  (:require [chess.bitboard.impl.moves :as moves])
  (:require [chess.bitboard.api :as api])
  (:use [clojure.test]))

(deftest test-generate-moves
  (testing "some moves")

  (=[[:R 0 56] [:R 0 48] [:R 0 40] [:R 0 32] [:R 0 24] [:R 0 16] [:R 0 8]
     [:R 0 7] [:R 0 6] [:R 0 5] [:R 0 4] [:R 0 3] [:R 0 2] [:R 0 1]]
     (moves/generate-moves (api/read-fen "8/8/8/8/8/8/8/R7 w KQkq - 0 1")))
  (=[[:P 23 31] [:P 22 30] [:P 21 29] [:P 20 28] [:R 17 57] [:R 17 49] [:R 17 41]
     [:R 17 33] [:R 17 25] [:R 17 19] [:R 17 18] [:R 17 16] [:R 17 9] [:R 17 1]]
     (moves/generate-moves (api/read-fen "8/8/8/8/8/1R11PPPP/8/8 w KQkq - 0 1"))))

(deftest test-pawn-moves
  (testing "pawn is allowed to move 2 steps up from initial position"
    (are [x y] (= x y)
         #{'(0 2) '(0 3)}  (apply hash-set (api/possible-moves api/initial-board [0 1]))
         #{'(0 5) '(0 4)}  (apply hash-set (api/possible-moves api/initial-board [0 6]))))
  (testing "pawn is allowed to move 1 step up from any other position"
    (are [x y] (= x y)
         '((0 3)) (api/possible-moves (api/move-piece api/initial-board [0 1] [0 2]) [0 2])
         '((0 4)) (api/possible-moves (api/move-piece api/initial-board [0 6] [0 5]) [0 5])))
  (testing "no moves if pawn is blocked"
    (is (= '() (api/possible-moves (api/move-piece api/initial-board [0 6] [0 2]) [0 1]))))
  (testing "pawn attacks diagonal"
    (is (= #{'(0 2) '(0 3) '(1 2)}
           (apply hash-set (api/possible-moves
                            (api/move-piece api/initial-board [1 6] [1 2]) [0 1]))))))

(deftest test-queen-moves
  (let [game-state (-> api/initial-board
                       (api/move-piece [3 1] [4 4])
                       (api/move-piece [2 1] [2 2]))]
    (is (= #{[2 1] [1 2] [0 3] [3 1] [3 2] [3 3] [3 4] [3 5] [3 6]}
           (apply hash-set (api/possible-moves game-state [3 0]))))))

(deftest test-king-moves
  (let [game-state (-> api/initial-board
                       (api/move-piece [4 1] [4 2])
                       (api/move-piece [5 1] [5 2]))]
    (is (= #{[5 1] [4 1]} (apply hash-set (api/possible-moves game-state [4 0]))))))

(deftest test-rook-moves
  (let [game-state (-> api/initial-board
                       (api/move-piece [0 1] [0 6])
                       (api/move-piece [1 0] [1 6]))]
    (is (= #{[0 1] [0 2] [0 3] [0 4] [0 5] [1 0]}
           (apply hash-set (api/possible-moves game-state [0 0]))))))

(deftest test-bishop-moves
  (let [game-state (-> api/initial-board
                       (api/move-piece [1 1] [1 5])
                       (api/move-piece [3 1] [3 5])) ]
    (is (= #{[1 1] [0 2] [3 1] [4 2] [5 3] [6 4] [7 5]}
           (apply hash-set (api/possible-moves game-state [2 0])))))

  (let [game-state (-> api/initial-board
                       (assoc :turn :b)
                       (api/move-piece [3 6] [1 1])
                       (api/move-piece [5 6] [1 1]))]
    (is (=  #{[5 4] [4 5] [3 6]  [7 2] [6 3]}
            (apply hash-set (api/possible-moves game-state [2 7]))))))

(deftest test-knight-moves
  (is (= #{[2 2] [0 2]} (apply hash-set (api/possible-moves api/initial-board [1 0]))))
