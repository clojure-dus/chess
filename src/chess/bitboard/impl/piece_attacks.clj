(ns chess.bitboard.impl.piece-attacks
  (:use [chess.bitboard.impl.file-rank])
  (:use [chess.bitboard.impl.bitoperations]))

; pre-calculated move and attack arrays

(defn- create-vect-bitboards [moves-coords]
  "creates a vector of  64 squares which have bitboards
   in which moves-coords  have been flaged "
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

(defn indexed-bits [bit-vector]
  "gets the one bits from a vector of zero/ones into a vector of [pos 1]
  For example [1 0 1] -> [[0 1] [2 1]]"
 (filter (fn[[idx bit]] (= 1 bit)) (map-indexed vector  bit-vector)))

(defn slide-attack-byte [occupied-bits pos]
  "occupied-bits -  flags the positions of all pieces of a rank,file or diagonal.
   pos  -  the current square  position of the current piece
   returns  a  long where the first 8-bits flag the squares to which an piece on position pos
   can move (including any attacked pieces)
   Example : (slide-attack-bits 2r001001 3) -> 2r10110

   occupied-bits can be between 0 and 64 which means that 6-bits are sufficient to
   represent all occupied states of any row,column or diagonal of a chessboard.
   The 2 outer bits a1 and h1 are not needed because they depend on b1 and g1.
   thats why six bits are sufficeient."

  (let [occupied-bits (bit-shift-left occupied-bits 1)
        bit-vect (bit->vector occupied-bits 8)
        indexed-bits (indexed-bits  bit-vect)
        nearest-left  (nth (last  (filter (fn [[idx bit]] (< idx pos)) indexed-bits)) 0 0)
        nearest-right (nth (first (filter (fn [[idx bit]] (> idx pos)) indexed-bits)) 0 9)]
    (vector->bit (for [idx (range 8)]
                             (cond (= idx pos)           0
                                   (< idx nearest-left)  0
                                   (> idx nearest-right) 0
                                   :else                 1)))))


(defn make-attack-diagonal [square occupied diagonal-row-getter]
"returns a diagonal sliding attack mask"
  (let [diag-row    (diagonal-row-getter square)
        position    (.indexOf diag-row square)
        attack-bits (slide-attack-byte occupied position)
        attack-vect (bit->vector attack-bits 8)
        update-vect (partition 2 (interleave diag-row attack-vect))]
     (reduce (fn [bitmap [sq bit]]
               (if (= 1 bit) (bit-set bitmap sq) bitmap)) 0
               update-vect)))

;;--------------------------------------------------------------------
;; Pre-calculated attack and move arrays for each chess piece
;; which  are used by move.clj to calculate possible of an piece
;;--------------------------------------------------------------------

(def knight-attack-array
  "creates a lookup array of 64 squares which have bitboards
   in which knight-attacks have been flaged "
  (long-array (create-vect-bitboards
              [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]] )))

 (def pawn-white-move-array
 "creates a lookup array of 64 squares which have bitboards
   in which white pawn moves have been flaged "
   (long-array  (create-vect-bitboards [[0 1]])))

(def pawn-white-double-move-array
   "creates a lookup array of 64 squares which have bitboards
   in which the white double moves have been flaged "
   (let [row-4-squares [24 25 26 27 28 29 30 31]]
     (long-array (concat (repeat 8 0)
                         (map #(bit-set 0 %) row-4-squares)
                         (repeat 48 0)))))

(def pawn-black-move-array
 "creates a lookup array of 64 squares which have bitboards
   in which black pawn moves have been flaged "
  (long-array  (create-vect-bitboards [[0 -1]])))

(def pawn-black-double-move-array
   "creates a lookup array of 64 squares which have bitboards
   in which the black double moves have been flaged "
  (let [row-5-squares [32 33 34 35 36 37 38 39]]
     (long-array (concat (repeat 48 0)
                         (map #(bit-set 0 %) row-5-squares)
                         (repeat 8 0)))))

(def pawn-white-attack-array
   "creates a lookup array of 64 squares which have bitboards
   in which white pawn attacks have been flaged "
  (long-array (create-vect-bitboards [[1 1] [-1 1]])))

(def pawn-black-attack-array
  "creates a lookup array of 64 squares which have bitboards
   in which black pawn attacks have been flaged "
  (long-array (create-vect-bitboards [[-1 -1] [1 -1]])))

(def king-attack-array
  "creates a lookup array of 64 squares which have bitboards
   in which king attacks have been flaged "
  (long-array (create-vect-bitboards [[1 1][-1 1][-1 -1][1 -1][0 1][0 -1][1 0][-1 0]])))

(def attack-array-ranks
  "2 dimensinal array [square][occupied] in which all horizontal slide positions
   have been flaged. occupied is a 6-bit (0..64) mask having 1 bit flags
   for all occupied squares of a row."
  (to-long-array-2d
   (map (fn [square]
          (let [shift-left #(bit-shift-left %2 (aget ^ints rank-shift-array %1))]
            (for [occupied (range 64)]
              (shift-left square
                 (slide-attack-byte occupied (square->column square))))))
        (range 64))))

(defn ^long get-attack-rank [game-state  ^long from-sq]
  (let [^long allpieces        (:allpieces game-state)
        occupied-row           (bit-and allpieces (aget ^longs masks-row from-sq))
        ^long occupied-mask    (unsigned-shift-right occupied-row
                                      (inc (aget ^ints rank-shift-array from-sq)))]
        (deep-aget ^longs attack-array-ranks from-sq occupied-mask)))

(def attack-array-files
  "2 dimensinal array [square][occupied] in which all vertical slide positions
   have been flaged. occupied is a 6-bit (0..64) mask having 1 bit flags
   for all occupied squares of a column."
(to-long-array-2d
 (map (fn [square]
        (let [ shift-column-left #(bit-shift-left %2 (square->column %1))]
          (for [occupied (range 64)]
            (shift-column-left square
               (rotate90-bitboard-clockwise
                  (slide-attack-byte occupied (- 7 (square->row square))))))))
        (range 64))))

(defn ^long get-attack-file [game-state ^long from-sq]
  (let [^long allpieces        (:allpieces game-state)
        occupied-column        (bit-and allpieces (aget ^longs masks-column from-sq))
        ^long occupied-mask    (shift-rank-to-bottom occupied-column from-sq)]
    (deep-aget ^longs attack-array-files from-sq occupied-mask)))


(def attack-array-diagonal-a1h8
   "2 dimensinal array [square][occupied] in which all diagonal slide positions from
  a1 to h8 have been flaged. occupied is a 6-bit (0..64) mask having 1 bit flags
   for all occupied squares of a diagonal."
 (to-long-array-2d
   (map (fn [square]
      (for [occupied (range 64)]
         (make-attack-diagonal square occupied get-diagonal-a1h8)))
           (range 64))))

(defn ^long get-attack-diagonal-a1h8 [game-state ^long from-sq]
   (let [^long allpieces        (:allpieces game-state)
         occupied-diagonal      (bit-and allpieces (aget ^longs masks-diagonal-a1h8 from-sq))
         ^long occupied-mask    (shift-diagonal-a1h8-to-bottom occupied-diagonal from-sq)]
     (deep-aget ^longs attack-array-diagonal-a1h8 from-sq occupied-mask)))

(def attack-array-diagonal-a8h1
   "2 dimensinal array [square][occupied] in which all diagonal slide positions from
  a8 to h1 have been flaged. occupied is a 6-bit (0..64) mask having 1 bit flags
   for all occupied squares of a diagonal."
  (to-long-array-2d
   (map (fn [square]
          (for [occupied (range 64)]
            (make-attack-diagonal square occupied get-diagonal-a8h1)))
        (range 64))))

(defn ^long get-attack-diagonal-a8h1 [game-state ^long from-sq]
  (let [^long allpieces      (:allpieces game-state)
        occupied-diagonal    (bit-and allpieces (aget ^longs masks-diagonal-a8h1 from-sq))
        ^long occupied-mask  (shift-diagonal-a8h1-to-bottom occupied-diagonal from-sq)]
             (deep-aget ^longs attack-array-diagonal-a8h1 from-sq occupied-mask)))
