(ns chess.bitboard.api
  (:use [chess.bitboard.impl.chessboard
         :only (move-piece initial-board read-fen)
         :rename {move-piece impl-move-piece
                  read-fen impl-read-fen
                  initial-board impl-initial-board}])
  (:use [chess.bitboard.impl.file-rank
         :only (lookup-file lookup-rank)])
  (:use [chess.bitboard.impl.moves
         :only  (generate-moves find-piece-moves)
         :rename {generate-moves impl-generate-moves}]))

(defn- coord->square [[file rank]]
  (- (+ (* 8 (inc rank)) (inc file)) 9))

(defn- square->coord [square]
  [(dec (aget lookup-file square)) (dec (aget lookup-rank square))])

(defn move-piece [game-state from dest]
   "move piece by coordinates from and dest"
   (let [from-sq (coord->square from)
         dest-sq (coord->square dest)
         piece   (nth (:board game-state) from-sq)]
     (impl-move-piece game-state piece from-sq dest-sq)))

(defn generate-moves [game-state]
  (impl-generate-moves game-state))

(defn possible-moves [game-state coord]
  (let [square (coord->square coord)
        piece  (nth (:board game-state) square)]
    (map  (fn[[_ _ dest]] (square->coord dest))  (find-piece-moves piece square game-state))))

(def initial-board impl-initial-board)

(defn read-fen [str]
  (impl-read-fen str))
