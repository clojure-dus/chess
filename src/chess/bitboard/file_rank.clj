(ns chess.bitboard.file-rank
  ( :use [chess.bitboard.bitoperations]))

(def file-squares  (take 64 (cycle (range 1 9))))

(def  lookup-file  ^ints (into-array Integer/TYPE file-squares))

(def rank-squares (mapcat (partial  repeat 8) (range 1 9)))

(def  lookup-rank  ^ints (into-array Integer/TYPE rank-squares))

(defn lookup-file-rank [square] [(aget lookup-file square) (aget lookup-rank square)])

(def file-rank-squares
  (map #(let [file (aget lookup-file %)
              rank (aget lookup-rank %)]
                [% file rank ])(range 64)))

(defn coord->square [file rank]
    (- (+ (* 8 rank) file) 9))

(defn square->coord [square]
  [( aget lookup-file square) (aget lookup-rank square)])

(defn coords->squares [coords]
   (map (fn [[f r]] ( coord->square f r)) coords))

(def rank-shift-array
  (into-array Integer/TYPE
              (mapcat #(repeat 8 %) (take 8 (iterate (partial + 8) 0)))))

(defn- row-masks-vector[]
  (map #(bit-shift-left 2r01111110 (* 8 (dec %))) rank-squares))

(def ^:longs masks-row
  "used to get a specific rank row. array has for each"
  (into-array Long/TYPE (row-masks-vector)))


(def ^:longs masks-column
  "used to get a specific rank row. array has for each"
  (into-array Long/TYPE
      (let [left-column 2r0000000000000001000000010000000100000001000000010000000100000000]
         (map #(bit-shift-left left-column  (dec % )) file-squares))))

(def ^:longs masks-diagonal-a1h8
  (into-array Long/TYPE
     (map-indexed (fn [sq bits] (rotate-bitboard-45-anticlockwise sq bits)) (row-masks-vector))))
 (aget masks-diagonal-a1h8 18)
(def ^:longs masks-diagonal-a8h1
  (into-array Long/TYPE
    (map-indexed (fn [sq bits] (rotate-bitboard-45-clockwise sq bits)) (row-masks-vector))))

(defn square->column[square] (dec (aget lookup-file square)))

(defn square->row[square] (dec (aget lookup-rank square)))

(def magics-file
  (into-array Long/TYPE
              (apply concat (repeat 8 [(unchecked-long 0x8040201008040200)
                                 (unchecked-long 0x4020100804020100)
                                 (unchecked-long 0x2010080402010080)
                                 (unchecked-long 0x1008040201008040)
                                 (unchecked-long 0x0804020100804020)
                                 (unchecked-long 0x0402010080402010)
                                 (unchecked-long 0x0201008040201008)
                                 (unchecked-long 0x0100804020100804)]))))

(def lookup-diagonal-a1h8-old [
        7  8  9 10 11 12 13 14
        6  7  8  9 10 11 12 13
        5  6  7  8  9 10 11 12
        4  5  6  7  8  9 10 11
        3  4  5  6  7  8  9 10
        2  3  4  5  6  7  8  9
        1  2  3  4  5  6  7  8
        0  1  2  3  4  5  6  7
                           ])

(def magics-diagonal-a8h1
  (into-array Long/TYPE
              (let [magics [0x0                  ;a1
                            0x0
                            0x0101010101010100   ;a2-b1
                            0x0101010101010100
                            0x0101010101010100
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
  (into-array Long/TYPE
              (let [magics [0x0                  ;a8
                            0x0
                            0x0101010101010100   ;a6-c8
                            0x0101010101010100
                            0x0101010101010100
                            0x0101010101010100
                            0x0101010101010100
                            0x0101010101010100
                            0x0080808080808080  ;b1-h7
                            0x0040404040404040
                            0x2020202020000000  ;d1-h5
                            0x1010101000000000  ;e1-h4
                            0x0808080000000000  ;f1-h3
                            0x0
                            0x0]]

                (map #(nth magics %)
                     [ 7   6  5  4  3  2  1  0
                       8   7  6  5  4  3  2  1
                       9   8  7  6  5  4  3  2
                      10   9  8  7  6  5  4  3
                      11  10  9  8  7  6  5  4
                      12  11 10  9  8  7  6  5
                      13  12 11 10  9  8  7  6
                      14  13 12 11 10  9  8  7]))))


(defmacro shift-rank-to-bottom[bitboard square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget magics-file ~square))) 57))


(defmacro shift-diagonal-a1h8-to-bottom[bitboard square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget magics-diagonal-a1h8 ~square))) 57))



(defmacro shift-diagonal-a8h1-to-bottom[bitboard square]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard)
                        (unchecked-long (aget magics-diagonal-a8h1 ~square))) 57))
