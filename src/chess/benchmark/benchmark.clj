(ns chess.benchmark.benchmark
  (:use [chess.core.core])
  (:use [chess.movelogic.move-generator])
  (:use [chess.ai.move-selection.min-max])
  (:use [chess.ai.rating.board-rating :only (rate)]))

(def boards-fen  ["8/5k2/6P1/8/8/8/8/2K5 b - - 0 1" "8/5k2/4P3/8/8/8/8/2K5 b - - 0 1" "4k3/8/8/8/8/2p5/3K4/8 w - - 0 1"  "4k3/8/8/8/8/4p3/3K4/8 w - - 0 1"  "8/5k2/8/8/8/8/8/2K2Q2 b - - 0 1"  "8/5k2/8/8/8/8/Q7/2K5 b - - 0 1" "8/5k2/6B1/8/8/8/8/2K5 b - - 0 1" "8/5k2/8/8/8/8/B7/2K5 b - - 0 1" "8/5k2/8/8/2R5/8/B7/2K5 b - - 0 1" "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - - 0 1" "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2" "r4rnk/1pp4p/3p4/3P1b2/1PPbpBPq/8/2QNB1KP/1R3R2 b - - 0 23" "8/1p6/1P1p4/1B1Pk2p/8/7K/8/4r3 b - - 0 0" "r1b2rk1/ppp1qpp1/2np1n1p/1B2p3/4P2B/2PP1N2/P1PQ1PPP/R4RK1 w KQkq - - 0 50" "8/K1P2n1p/PQP2pkp/P1P2p1p/8/8/8/8 w - - 0 1" "7K/8/k1P5/7p/8/8/8/8 w KQkq - - 0 0" "4k3/8/8/8/2p5/8/3P4/5K2 w KQkq - - 0 0" "r5rk/5p1p/5R2/4B3/8/8/7P/7K w KQkq - - 0 0" "5B2/6P1/1p6/8/1N6/kP6/2K5/8 w KQkq - - 0 0" "3n2nr/4Pqpp/2k5/8/8/8/2B3PP/6K1 w KQkq - - 0 1" "8/R7/4kPP1/3ppp2/3B1P2/1K1P1P2/8/8 w KQkq - - 0 1" "r1bq2r1/b4pk1/p1pp1p2/1p2pP2/1P2P1PB/3P4/1PPQ2P1/R3K2R w KQkq - - 0 1" "3r1r1k/1p3p1p/p2p4/4n1NN/6bQ/1BPq4/P3p1PP/1R5K w KQkq - - 0 1" ])



(def allboards (flatten (repeat (map read-fen boards-fen))))

(defn measure [x f allboards]
  (let [ boards (take x allboards) ]
    (println  (time (doseq [board boards] (f board))))))

(defn measure-moves []
  (measure 400  generate-moves allboards ))


(defn measure-rating []
  (measure 400 rate allboards))

(defn measure-move-selection []
  (measure 1 select-move allboards))

(binding [*move-engine* bitboard-engine]
      (measure-rating))

(comment
  (binding [*move-engine* bitboard-engine]
    (measure-move-selection)))

(binding [*move-engine* vector-engine]
      (measure-rating))

(comment
  (binding [*move-engine* vector-engine]
    (measure-move-selection)))

(binding [*move-engine* vector-engine]
  (measure-moves))
