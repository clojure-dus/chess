(ns chess.bitboard.api
  (:require   [chess.bitboard.impl.moves
         :only  (generate-moves find-piece-moves check?)
         :as moves ])
  (:require [chess.bitboard.impl.chessboard
         :only (move-piece initial-board read-fen print-board)
             :as chessboard])
  (:require [chess.move-generator :as gen])
  
  (:use [chess.bitboard.impl.file-rank
         :only (lookup-file lookup-rank)]))

(defn- coord->square [[file rank]]
  (- (+ (* 8 (inc rank)) (inc file)) 9))

(defn- square->coord [square]
  [(dec (aget ^ints lookup-file square)) (dec (aget ^ints lookup-rank square))])

(defn native->coords [col]
  (mapv (fn [[piece from-sq dest-sq & promotion]]
         (vector (square->coord from-sq) (square->coord dest-sq))
         ) col))

(defn- move-piece-coord [game-state from-coord dest-coord]
   "move piece by coordinates from and dest"
   (let [from-sq (coord->square from-coord)
         dest-sq (coord->square dest-coord)
         piece   (nth (:board game-state) from-sq)]
     (chessboard/move-piece game-state piece from-sq dest-sq)))

(defn- generate-moves-coord [game-state]
  (map (fn [[p from dest & data]]
         (list (square->coord from) (square->coord dest)))
       (moves/generate-moves game-state)))

(defn native-generate-moves [game-state]
  " returns moves in the form ([:P 56 57].....)"
  (moves/generate-moves game-state))


(defn native-move-piece [game-state piece from-sq dest-sq promotion]
   "move by squares"
   (chessboard/move-piece game-state piece from-sq dest-sq promotion))



(defn possible-moves [game-state coord]
  (let [square (coord->square coord)]
    (map  (fn[[_ _ dest]] (square->coord dest))
          (moves/possible-moves game-state (bit-set 0 square)))))

(defn piece-at [game-state coord]
  (get-in chessboard/initial-board [:board (coord->square coord) ]))

(defn check? [game-state]
  (true? (moves/check? game-state)))

(def initial-board chessboard/initial-board)

(defn read-fen [str]
  (chessboard/read-fen str))

(defn print-board [game-state]
  (chessboard/print-board game-state))


(defn move-generator []
  (reify gen/MoveGenerator
    (generate-moves [this game-state]
      (generate-moves-coord game-state))

    (move-piece [this game-state from to]
      (move-piece-coord game-state from to))

    (test-check? [this game-state]
       (check? game-state))

    (read-fen [this str]
       (read-fen str))))
