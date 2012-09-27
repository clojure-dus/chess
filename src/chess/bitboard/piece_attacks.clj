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


;; sliding moves
;;

(defn slide-attack-8-bits [occupied-bits pos]
  "occupied-bits -  flags the positions of all pieces of a rank,file or diagonal.
   pos  -  the current square  position of the current piece
   returns  a  long where the first 8-bits flag the squares to which an piece on position pos
   can move (including any attacked pieces)
   Example : (slide-attack-bits 2r10001001 5) -> 2r00001011
"
  (let [bit-vect (bit->vector occupied-bits 8)
        indexed-bits (filter (fn[[idx bit]] (= 1 bit)) (map-indexed vector bit-vect))
        nearest-left  (nth (last  (filter (fn [[idx bit]] (< idx pos)) indexed-bits)) 0 0)
        nearest-right (nth (first (filter (fn [[idx bit]] (> idx pos)) indexed-bits)) 0 9)]
    (vector->bit (reverse (for [idx (range 8)]
                     (cond (= idx pos)           0
                           (< idx nearest-left)  0
                           (> idx nearest-right) 0
                           :else                 1))))))

(def all-slide-attacks-8-bits
  "an vector off  8 file rows  consisting of 256 possible slide attack 8-bits"
  (partition 256
             (for [pos (range 8) occupied (range 256)]
               (slide-attack-8-bits occupied pos))))

(defn shift-bits-to-square[square bits]
    (bit-shift-left bits (aget rank-shift-array square)))

(def rank-attack-array
  (to-array-2d
   (map (fn [square]
          (let [shift-fn (partial shift-bits-to-square square)
                column   (dec (aget lookup-file square))]
          (map shift-fn  (nth all-slide-attacks-8-bits column)))) (range 64))))

  (for [pos (range 1) occupied (range 256)]
    (slide-attack-8-bits occupied pos))
