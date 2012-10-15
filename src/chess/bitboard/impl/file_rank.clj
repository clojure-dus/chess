(ns chess.bitboard.impl.file-rank
  ( :use [chess.bitboard.impl.bitoperations]))

(def file-squares
  "vector where each item shows the column number"
  (take 64 (cycle (range 1 9))))

(def  lookup-file  ^ints (into-array Integer/TYPE file-squares))

(def   rank-squares
"vector where each item shows rank number"
  (mapcat (partial  repeat 8) (range 1 9)))

(def  lookup-rank  ^ints (into-array Integer/TYPE rank-squares))

(defn lookup-file-rank [square]
"maps a square 0 63 to a coord. for example 63 -> [8 8]"
[(aget ^ints lookup-file square) (aget ^ints lookup-rank square)])

(def file-rank-squares
  "vect of [square file rank]"
  (map #(let [file (aget ^ints lookup-file %)
              rank (aget ^ints lookup-rank %)]
                [% file rank ])(range 64)))

(defn coord->square [file rank]
  "map coord to square [8 8] -> 63"
    (- (+ (* 8 rank) file) 9))
(defn square->coord [square]
  "map square to coord 63 -> [8 8]"
  [( aget ^ints lookup-file square) (aget ^ints lookup-rank square)])

(defn coords->squares [coords]
  "map vect of coords to seq of sqaures"
  (map (fn [[f r]] ( coord->square f r)) coords))

(defn square->column[square] (dec (aget ^ints lookup-file square)))

(defn square->row[square] (dec (aget ^ints lookup-rank square)))

(def rank-shift-array
  "lookup the number to shift a row to the bottom"
  (into-array Integer/TYPE
              (mapcat #(repeat 8 %) (take 8 (iterate (partial + 8) 0)))))

(def ^:longs masks-row
  "used to get a specific rank row. array has for each square a maskof six bits set.
   (only using six bits is a optimization since the outer 2 bits do not count in
    attack calculation)"
  (into-array Long/TYPE (map #(bit-shift-left 2r01111110 (* 8 (dec %))) rank-squares)))

(def ^:longs masks-column
  "used to get a specific file column. array has for each sqaure the column bit set
 (only using six inner  bits is a optimization since the outer 2 bits do not count in
    attack calculation)"
  (into-array Long/TYPE
              (let
                  [left-column
                   2r0000000000000001000000010000000100000001000000010000000100000000]
         (map #(bit-shift-left left-column  (dec % )) file-squares))))


(def ^:const ^long mask-rochade-white-king  2r01100000)
(def ^:const ^long mask-rochade-white-queen 2r00001100)
(def ^:const ^long mask-rochade-black-king
  2r0110000000000000000000000000000000000000000000000000000000000000)
(def ^:const ^long mask-rochade-black-queen
  2r0000110000000000000000000000000000000000000000000000000000000000)

(def ^:const ^long move-rochade-white-king  2r00000100)
(def ^:const ^long move-rochade-white-queen 2r01000000)

(def ^:const ^long move-rochade-black-queen
  2r0000010000000000000000000000000000000000000000000000000000000000)
(def ^:const ^long move-rochade-black-king
  2r0100000000000000000000000000000000000000000000000000000000000000)



(def diagonals-a8h1
  "gives for each diagonal numbered 0..14 to the member squares "
                     [[0]
                      [8 1]
                      [16 9 2]
                      [24 17 10 3]
                      [32 25 18 11 4]
                      [40 33 26 19 12 5]
                      [48 41 34 27 20 13 6]
                      [56 49 42 35 28 21 14 7]
                      [57 50 43 36 29 22 15]
                      [58 51 44 37 30 23]
                      [59 52 45 38 31]
                      [60 53 46 39]
                      [61 54 47]
                      [62 55]
                      [63]])


(def diagonals-a1h8
  "gives for each diagonal numbered 0..14 to the member squares "
                   [[56]
                    [48 57]
                    [40 49 58]
                    [32 41 50 59]
                    [24 33 42 51 60]
                    [16 25 34 43 52 61]
                    [8 17 26 35 44 53 62]
                    [0 9 18 27 36 45 54 63]
                    [1 10 19 28 37 46 55]
                    [2 11 20 29 38 47]
                    [3 12 21 30 39]
                    [4 13 22 31]
                    [5 14 23]
                    [6 15]
                    [7]])

(defn get-diagonal-a8h1 [sq]
    "returns vect all  sqaures  belonging to the same diagonal as sq is on"
    (first (filter #(>(.indexOf % sq) -1) diagonals-a8h1) ))
(get-diagonal-a8h1 30)

(defn get-diagonal-a1h8 [sq]
      "returns vect all  sqaures  belonging to the same diagonal as sq is on"
  (first (filter #(>(.indexOf % sq) -1) diagonals-a1h8)))


(defn board-frame?
    "returns true if sq is on the outer border of the chess board"
  [sq] (contains? #{ 0 1 2 3 4 5 6 7
                                   15 23 31 39 47 55 63
                                   62 61 60 59 58 57 56
                                    48 40 32 24 16 8} sq))

(def ^:longs masks-diagonal-a8h1
  "gets for wach square a  diagonal mask where all 6 attack  relevant bits are set"
    (into-array Long/TYPE
                (let [find-diag (fn [[sq file rank]]
                                  (let [index      (- (+ file rank) 2)
                                        diagonal   (reverse (nth  diagonals-a8h1 index))
                                        inner-diag (filter (complement board-frame?) diagonal)]
                                    (reduce #(bit-set %1 %2) 0 inner-diag)))]
                  (map find-diag file-rank-squares))))

(def ^:longs masks-diagonal-a1h8
    "gets for wach square a  diagonal mask where all 6 attack  relevant bits are set"
  (into-array Long/TYPE
              (let [find-diag (fn [[sq file rank]]
                                (let [index (+ (- file rank) 7)
                                      diagonal (reverse (nth  diagonals-a1h8 index))
                                      inner-diag (filter (complement board-frame?) diagonal)]
                                  (reduce #(bit-set %1 %2) 0 inner-diag)))]
                (map find-diag file-rank-squares))))


(def ^:longs magics-file
  "used to slide a set of column 6 bits to the the horizontal bottom of the board"
  (into-array Long/TYPE
              (apply concat (repeat 8 [(unchecked-long 0x8040201008040200)
                                 (unchecked-long 0x4020100804020100)
                                 (unchecked-long 0x2010080402010080)
                                 (unchecked-long 0x1008040201008040)
                                 (unchecked-long 0x0804020100804020)
                                 (unchecked-long 0x0402010080402010)
                                 (unchecked-long 0x0201008040201008)
                                 (unchecked-long 0x0100804020100804)]))))

(def magics-diagonal-a8h1
    "used to slide a set of diagonal 6  bits to the the horizontal bottom of the board"
  (into-array Long/TYPE
              (let [magics [0x0                  ;a1
                            0x0
                            0x0101010101010100   ;a2-b1
                            0x0101010101010100
                            0x0101010101010100   ;a5-e1
                            0x0101010101010100
                            0x0101010101010100
                            0x0101010101010100
                            0x0080808080808080  ;b8-h2
                            0x0040404040404040
                            0x0020202020202020  ;d8-h4
                            0x0010101010101010  ;e8-h5
                            0x0008080808080808  ;f8-h6
                            0x0
                            0x0]]

                (map #(nth magics %)
                     [0  1  2  3  4  5  6  7
                      1  2  3  4  5  6  7  8
                      2  3  4  5  6  7  8  9
                      3  4  5  6  7  8  9 10
                      4  5  6  7  8  9 10 11
                      5  6  7  8  9 10 11 12
                      6  7  8  9 10 11 12 13
                      7  8  9 10 11 12 13 14]))))

(def magics-diagonal-a1h8
      "used to slide a set of diagonal 6  bits to the the horizontal bottom of the board"
  (into-array Long/TYPE
              (let [magics [0x0                  ;a8
                            0x0
                            0x0101010101010100   ;a6-c8
                            0x0101010101010100
                            0x0101010101010100
                            0x0101010101010100   ;a3-f8
                            0x0101010101010100  ;a2-g8
                            0x0101010101010100  ;a1-h8
                            0x0080808080808080  ;b1-h7
                            0x0040404040404040
                            0x2020202020000000  ;d1-h5
                            0x1010101000000000  ;e1-h4
                            0x0808080000000000  ;f1-h3
                            0x0
                            0x0]]

                (map #(nth magics %)
                     [ 7   8  9 10 11 12 13 14
                       6   7  8  9 10 11 12 13
                       5   6  7  8  9 10 11 12
                       4   5  6  7  8  9 10 11
                       3   4  5  6  7  8  9 10
                       2   3  4  5  6  7  8  9
                       1   2  3  4  5  6  7  8
                       0   1  2  3  4  5  6  7]))))

(defmacro shift-rank-to-bottom[bitboard square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget  ~'^longs magics-file ~square))) 57))

(defmacro shift-diagonal-a1h8-to-bottom[bitboard square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget  ~'^longs magics-diagonal-a1h8 ~square))) 57))

(defmacro shift-diagonal-a8h1-to-bottom[bitboard  square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget   ~'^longs magics-diagonal-a8h1 ~square))) 57))
