(ns chess.bitboard.piece-attacks
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations]))

(def  knight-attacks-array
  "creates a lookup array of  64 squares which have bitboards
   in which knightattacks have been flaged "
  (let [result (make-array Long/TYPE 65)
        knight-moves   [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]]

        all-moves  (for [[square file rank] file-rank-squares
                         [x y] knight-moves
                         :let  [f (+ file x) r (+ rank y)]
                         :when (and (> f 0 ) (< f 9) (> r 0) (< r 9))]
                     [square f r])
        ](doseq [[square f r] all-moves]
                       (let [b (aget result square)
                             bit (square->bit (coord->square f r))]
                         (aset result square (bit-or b bit))))
         result))
