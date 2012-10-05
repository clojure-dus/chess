(ns chess.benchmark
  (:use [chess.fen])
  (:use [chess.move-generator])
  (:require [chess.bitboard.api :as bb])
  (:import [chess.bitboard.impl.BitOps])
  (:use [chess.move-selection]))

(def allboards (flatten (repeat (map read-fen ["8/5k2/6P1/8/8/8/8/2K5 b - - 0 1" "8/5k2/4P3/8/8/8/8/2K5 b - - 0 1" "4k3/8/8/8/8/2p5/3K4/8 w - - 0 1"  "4k3/8/8/8/8/4p3/3K4/8 w - - 0 1"  "8/5k2/8/8/8/8/8/2K2Q2 b - - 0 1"  "8/5k2/8/8/8/8/Q7/2K5 b - - 0 1" "8/5k2/6B1/8/8/8/8/2K5 b - - 0 1" "8/5k2/8/8/8/8/B7/2K5 b - - 0 1" "8/5k2/8/8/2R5/8/B7/2K5 b - - 0 1" "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - - 0 1" "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2" "r4rnk/1pp4p/3p4/3P1b2/1PPbpBPq/8/2QNB1KP/1R3R2 b - - 0 23" "8/1p6/1P1p4/1B1Pk2p/8/7K/8/4r3 b - - 0 52" ]))))

(defn measure [x f]
  (let [ boards (take x allboards) ]
    (println  (time (doseq [board boards] (f board))))))

(defn measure-moves []
  (measure 1000 generate-moves))

(defn measure-bitmap-moves []
  (measure 1000 bb/generate-moves))

(defn measure-rating []
  (measure 10 select-move))