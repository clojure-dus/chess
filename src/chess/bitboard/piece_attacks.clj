(ns chess.bitboard.piece-attacks
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations]))

(defn- move-array [moves-coords]
  "creates a lookup array of  64 squares which have bitboards
   in which moves  have been flaged "
  (let [result (make-array Long/TYPE 64)

        all-moves  (for [[square file rank] file-rank-squares
                         [x y] moves-coords
                         :let  [f (+ file x) r (+ rank y)]
                         :when (and (> f 0 ) (< f 9) (> r 0) (< r 9))]
                     [square f r])
        ](doseq [[square f r] all-moves]
                       (let [b (aget result square)
                             bit (square->bit (coord->square f r))]
                         (aset result square (bit-or b bit))))
         result))

(def knight-attack-array
  "creates a lookup array of  64 squares which have bitboards
   in which knightattacks have been flaged "
  (move-array  [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]] ))

(defn bit-set-in-array [arr idx bit-index]
  (aset arr idx (bit-or (aget arr idx) (bit-set 0 bit-index))))

(def pawn-white-move-array
  (let [single-moves (move-array [[0 1]])
        row-2  (coords->squares [[1 2][2 2][3 2][4 2][5 2][6 2][7 2][8 2]])
        double-move-const 16]
    (doseq [square row-2]
      (bit-set-in-array single-moves square (+ square double-move-const)))
     single-moves))

(def pawn-black-move-array
  (let [single-moves (move-array [[0 -1]])
        row-7 (coords->squares [[1 7][2 7][3 7][4 7][5 7][6 7][7 7][8 7]])
        double-move-const 16]
    (doseq [square row-7]
      (bit-set-in-array single-moves square (- square double-move-const)))
    single-moves))

(def pawn-white-attack-array
  (move-array [[1 1] [-1 1]]))

(def pawn-black-attack-array
  (move-array [[-1 -1] [1 -1]]))

(def king-attack-array
  (move-array [[1 1][-1 1][-1 -1][1 -1][0 1][0 -1][1 0][-1 0]]))
