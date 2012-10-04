(ns chess.bitboard.api
  (:use [chess.bitboard.impl.chessboard

         :rename {move-piece impl-move-piece}])
  (:use [chess.bitboard.impl.file-rank])
  (:use [chess.bitboard.impl.moves
         :only  (generate-moves find-piece-moves)
         :rename {generate-moves impl-generate-moves}]))

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
        piece   (nth (:board game-state) square)]
    (find-piece-moves piece square game-state)))
