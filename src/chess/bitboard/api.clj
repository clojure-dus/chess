(ns chess.bitboard.api
  (:require [chess.bitboard.impl.chessboard
         :only (move-piece initial-board read-fen print-board)
         :as chessboard])
  (:use [chess.bitboard.impl.file-rank
         :only (lookup-file lookup-rank)])
  (:require  [chess.bitboard.impl.moves
         :only  (generate-moves find-piece-moves)
         :as moves]))

(defn- coord->square [[file rank]]
  (- (+ (* 8 (inc rank)) (inc file)) 9))

(defn- square->coord [square]
  [(dec (aget ^ints lookup-file square)) (dec (aget ^ints lookup-rank square))])

(defn move-piece [game-state from-coord dest-coord]
   "move piece by coordinates from and dest"
   (let [from-sq (coord->square from-coord)
         dest-sq (coord->square dest-coord)
         piece   (nth (:board game-state) from-sq)]
     (chessboard/move-piece game-state piece from-sq dest-sq)))

(defn generate-moves [game-state]
  (map (fn [[p from dest]]
         (list (square->coord from) (square->coord dest)))
       (apply concat (filter #(not (empty? %)) (moves/generate-moves game-state)))))


(defn possible-moves [game-state coord]
  (let [square (coord->square coord)
        piece  (nth (:board game-state) square)]
    (map  (fn[[_ _ dest]] (square->coord dest))
          (moves/find-piece-moves piece square game-state))))

(defn piece-at [game-state coord]
  (get-in chessboard/initial-board [:board (coord->square coord) ]))


(def initial-board chessboard/initial-board)

(defn read-fen [str]
  (chessboard/read-fen str))

(defn print-board [game-state]
  (chessboard/print-board game-state))
