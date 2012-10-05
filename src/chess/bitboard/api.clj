(ns chess.bitboard.api
  (:require [chess.bitboard.impl.chessboard
         :only (move-piece initial-board read-fen)
         :as chessboard])
  (:use [chess.bitboard.impl.file-rank
         :only (lookup-file lookup-rank)])
  (:require  [chess.bitboard.impl.moves
         :only  (generate-moves find-piece-moves)
         :as moves]))

(defn- coord->square [[file rank]]
  (- (+ (* 8 (inc rank)) (inc file)) 9))

(defn- square->coord [square]
  [(dec (aget lookup-file square)) (dec (aget lookup-rank square))])

(defn move-piece [game-state from dest]
   "move piece by coordinates from and dest"
   (let [from-sq (coord->square from)
         dest-sq (coord->square dest)
         piece   (nth (:board game-state) from-sq)]
     (chessboard/move-piece game-state piece from-sq dest-sq)))


(defn generate-moves [game-state]
  (map (fn [[p from dest]] (list (square->coord from) (square->coord dest)))
       (filter #(not (empty? %)) (moves/generate-moves game-state))))

(defn possible-moves [game-state coord]
  (let [square (coord->square coord)
        piece  (nth (:board game-state) square)]
    (map  (fn[[_ _ dest]] (square->coord dest))  (moves/find-piece-moves piece square game-state))))

(def initial-board chessboard/initial-board)

(defn read-fen [str]
  (chessboard/read-fen str))
