(ns chess.bitboard.piece-attacks
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations]))

(defn- create-vect-bitboards [moves-coords]
  "creates a vector of  64 squares which have bitboards
   in which moves  have been flaged "
  (let [all-moves  (for [[square file rank] file-rank-squares
                         [x y] moves-coords
                         :let  [f (+ file x) r (+ rank y)]
                         :when (and (> f 0 ) (< f 9) (> r 0) (< r 9))]
                     [square f r])

        bitboard-updater (fn [result [square f r]]
                           (let [bitboard  (nth result square)
                                 bit       (bit-set 0 (coord->square f r))]
                                 (assoc result square (bit-or bitboard bit))))

        empty-board-vect (apply vector (repeat 64 0))]
    (reduce bitboard-updater empty-board-vect all-moves)))

; result (make-array Long/TYPE 64)
(def knight-attack-array
  "creates a lookup array of  64 squares which have bitboards
   in which knightattacks have been flaged "
  (long-array (create-vect-bitboards
              [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]] )))


(defn update-in-bitboard  [distance bb-vector square]
     (let [bitboard    (nth bb-vector square)
           double-move (bit-set 0 (+ distance square))]
         (assoc bb-vector square (bit-or bitboard double-move))))


(def pawn-white-move-array
  (let [single-moves     (create-vect-bitboards [[0 1]])
        row-2-squares    [8 9 10 11 12 13 14 15]
        make-double-move (partial update-in-bitboard 16) ]
    (long-array (reduce make-double-move single-moves row-2-squares))))

(def pawn-black-move-array
  (let [single-moves     (create-vect-bitboards [[0 -1]])
        row-7-squares    [48 49 50 51 52 53 54 55]
        make-double-move (partial update-in-bitboard -16) ]
    (long-array (reduce make-double-move single-moves row-7-squares))))

(def pawn-white-attack-array
  (long-array (create-vect-bitboards [[1 1] [-1 1]])))

(def pawn-black-attack-array
  (long-array (create-vect-bitboards [[-1 -1] [1 -1]])))

(def king-attack-array
  (long-array (create-vect-bitboards [[1 1][-1 1][-1 -1][1 -1][0 1][0 -1][1 0][-1 0]])))


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
