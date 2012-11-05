(ns chess.test.bitboard.impl.moves

  (:use [chess.move-generator])
  (:require [chess.bitboard.api :as api])
  (:use [clojure.test]))

(deftest test-generate-moves
  (let [gen  (api/move-generator)]
    (testing "some moves")

    (=[[:R 0 56] [:R 0 48] [:R 0 40] [:R 0 32] [:R 0 24] [:R 0 16] [:R 0 8]
       [:R 0 7] [:R 0 6] [:R 0 5] [:R 0 4] [:R 0 3] [:R 0 2] [:R 0 1]]
      (generate-moves gen (read-fen gen  "8/8/8/8/8/8/8/R7 w KQkq - 0 1")))
    (=[[:P 23 31] [:P 22 30] [:P 21 29] [:P 20 28] [:R 17 57] [:R 17 49] [:R 17 41]
       [:R 17 33] [:R 17 25] [:R 17 19] [:R 17 18] [:R 17 16] [:R 17 9] [:R 17 1]]
      (generate-moves gen (read-fen gen "8/8/8/8/8/1R11PPPP/8/8 w KQkq - 0 1")))))

(deftest test-pawn-moves
  (let [gen  (api/move-generator)]
    (testing "pawn is allowed to move 2 steps up from initial position"
      (are [x y] (= x y)
           #{'(0 2) '(0 3)}  (apply hash-set (api/possible-moves api/initial-board [0 1]))
           #{'(0 5) '(0 4)}  (apply hash-set (api/possible-moves api/initial-board [0 6]))))
    (testing "pawn is allowed to move 1 step up from any other position"
      (are [x y] (= x y)
           '((0 3)) (api/possible-moves (move-piece gen api/initial-board [0 1] [0 2]) [0 2])
           '((0 4)) (api/possible-moves (move-piece gen api/initial-board [0 6] [0 5]) [0 5])))
    (testing "no moves if pawn is blocked"
      (is (= '() (api/possible-moves (move-piece gen api/initial-board [0 6] [0 2]) [0 1]))))
    (testing "pawn attacks diagonal"
      (is (= #{'(0 2) '(0 3) '(1 2)}
             (apply hash-set (api/possible-moves
                              (move-piece gen api/initial-board [1 6] [1 2]) [0 1])))))))
(deftest test-king-moves
  (let [gen  (api/move-generator)
                move (partial move-piece gen)
        game-state (-> api/initial-board
                       (move [4 1] [4 2])
                       (move [5 1] [5 2]))]
    (is (= #{[5 1] [4 1]} (apply hash-set (api/possible-moves game-state [4 0]))))))

(deftest test-knight-moves
  (is (= #{[2 2] [0 2]} (apply hash-set (api/possible-moves api/initial-board [1 0])))))

(deftest test-rook-moves
  (let [gen  (api/move-generator)
        move (partial move-piece gen)
        game-state (-> api/initial-board
                       (move [0 1] [0 6])
                       (move [1 0] [1 6]))]
    (is (= #{[0 1] [0 2] [0 3] [0 4] [0 5] [1 0]}
           (apply hash-set (api/possible-moves game-state [0 0]))))))

(deftest test-bishop-moves
  (let [gen  (api/move-generator)
         move (partial move-piece gen)
        game-state (-> api/initial-board
                       (move [1 1] [1 5])
                       (move [3 1] [3 5])) ]
    (is (= #{[1 1] [0 2] [3 1] [4 2] [5 3] [6 4] [7 5]}
           (apply hash-set (api/possible-moves game-state [2 0])))))

  (let [gen  (api/move-generator)
         move (partial move-piece gen)
        game-state (-> api/initial-board
                       (assoc :turn :b)
                       (move [3 6] [1 1])
                       (move [5 6] [1 1]))]
    (is (=  #{[5 4] [4 5] [3 6]  [7 2] [6 3]}
            (apply hash-set (api/possible-moves game-state [2 7]))))

    (is (= #{[3 6] [2 5] [1 4] [0 3]} (apply hash-set
                   (api/possible-moves
                    (read-fen gen "4B3/5P2/8/8/8/8/8/8 w KQkq - 0 1") [4 7]))))

    (is (= #{[3 6] [2 5] [1 4] [0 3]} (apply hash-set
                   (api/possible-moves
                    (read-fen gen "4B3/5P2/6P1/8/8/8/8/8 w KQkq - 0 1") [4 7]))))

    (is (= #{[3 6] [2 5] [1 4] [0 3] [5 6]} (apply hash-set
                   (api/possible-moves
                    (read-fen gen "4B3/5p2/6P1/8/8/8/8/8 w KQkq - 0 1") [4 7]))))


    (is (= #{[4 1][3 2][2 3][1 4][0 5]} (apply hash-set
                   (api/possible-moves
                    (read-fen gen "8/8/8/8/8/8/6P1/5B2 w KQkq - 0 1") [5 0]))))))

(deftest test-queen-moves
  (let [gen  (api/move-generator)
         move (partial move-piece gen)
        game-state (-> api/initial-board
                       (move [3 1] [4 4])
                       (move [2 1] [2 2]))]
    (is (= #{[2 1] [1 2] [0 3] [3 1] [3 2] [3 3] [3 4] [3 5] [3 6]}
           (apply hash-set (api/possible-moves game-state [3 0]))))))

(deftest test-rochade
(testing "some rochade (with attacks ")
(let [gen  (api/move-generator)
       move (partial move-piece gen)
      game-state (-> api/initial-board
                       (move [4 1] [4 3])
                       (move [5 0] [4 1])
                       (move [6 0] [5 2])
                       (move [3 0] [3 2])
                       (move [2 0] [3 1])
                       (move [2 1] [2 2])
                       (move [3 0] [2 1])
                       (move [1 0] [0 2]) )]
    (is (= #{[6 0] [5 0] [3 0] [2 0]}
           (apply hash-set (api/possible-moves game-state [4 0])))))
(let [gen  (api/move-generator)
       move (partial move-piece gen)
      game-state (-> api/initial-board
                       (move [6 6] [6 5])
                       (move [5 7] [6 6])
                       (move [6 7] [5 5])
                       (move [4 6] [4 5])
                       (move [3 7] [4 6])
                       (move [3 6] [3 5])
                       (move [2 7] [3 6])
                       (move [1 7] [2 5]))
       game-state (assoc game-state :turn :b)]
    (is (= #{[2 7] [3 7] [5 7] [6 7]}
           (apply hash-set (api/possible-moves game-state [4 7]))))
    (is (= #{[3 7] [5 7]}
           (apply hash-set (api/possible-moves
                            (read-fen gen "r3k2r/pppppppp/1N5N/8/8/8/8/8 b KQkq - 0 1") [4 7]))))
     (is (= #{[5 6] [2 7] [3 7] [5 7]}
           (apply hash-set (api/possible-moves
                            (read-fen gen "r3k2r/ppppp1pp/8/8/5R2/8/8/8 b KQkq - 0 1") [4 7]))))))

(deftest test-api-check?
  (let [gen  (api/move-generator)]
    (are [x y] (= x y)
         true (test-check? gen (read-fen gen "8/5k2/6P1/8/8/8/8/2K5 b - - 0 1"))
         true (test-check? gen (read-fen gen "8/5k2/4P3/8/8/8/8/2K5 b - - 0 1"))
         true (test-check? gen (read-fen gen "4k3/8/8/8/8/2p5/3K4/8 w - - 0 1"))
         true (test-check? gen(read-fen gen "4k3/8/8/8/8/4p3/3K4/8 w - - 0 1"))
         true (test-check? gen (read-fen gen "8/5k2/8/8/8/8/8/2K2Q2 b - - 0 1"))
         true (test-check? gen (read-fen gen "8/5k2/8/8/8/8/Q7/2K5 b - - 0 1"))
         false(test-check? gen (read-fen gen "8/5k2/8/8/2R5/8/Q7/2K5 w - - 0 1"))
         true (test-check? gen (read-fen gen "8/5k2/6B1/8/8/8/8/2K5 b - - 0 1"))
         true (test-check? gen (read-fen gen "8/5k2/8/8/8/8/B7/2K5 b - - 0 1"))
         false (test-check? gen (read-fen gen "8/5k2/8/8/2R5/8/B7/2K5 b - - 0 1")))))



(comment(run-tests))
