(ns chess.bitboard.file-rank)

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

(def file-letters-map { 1 "A" 2 "B" 3 "C" 4 "D" 5 "E" 6 "F" 7 "G" 8 "H"})

(defn square->coord [square]
  [(file-letters-map ( aget lookup-file square)) (aget lookup-rank square)])

(defn coords->squares [coords]
   (map (fn [[f r]] ( coord->square f r)) coords))

(def rank-shift-array
  (into-array Integer/TYPE
    (mapcat #(repeat 8 %) (take 8 (iterate (partial + 8) 0)))))

(def ^:longs row-masks
  "used to get a specific rank row. array has for each"
  (into-array Long/TYPE
              (map #(bit-shift-left 2r11111111 (* 8 (dec %))) rank-squares)))


(def ^:longs column-masks
  "used to get a specific rank row. array has for each"
  (into-array Long/TYPE
      (let [left-column 2r0000000100000001000000010000000100000001000000010000000100000001]
         (map #(bit-shift-left left-column  (dec % )) file-squares))))

(defn square->column[square] (dec (aget lookup-file square)))

(defn square->row[square] (dec (aget lookup-rank square)))

(def file-magic
  (into-array Long/TYPE
              [(unchecked-long 0x8040201008040200)
               (unchecked-long 0x4020100804020100)
               (unchecked-long 0x2010080402010080)
               (unchecked-long 0x1008040201008040)
               (unchecked-long 0x0804020100804020)
               (unchecked-long 0x0402010080402010)
               (unchecked-long 0x0201008040201008)
               (unchecked-long 0x0100804020100804)]))

(defmacro shift-column-to-bottom[bitboard column]
  `(~'unsigned-shift-right
    (unchecked-multiply (unchecked-long ~bitboard) (unchecked-long (aget file-magic ~column)))56))
