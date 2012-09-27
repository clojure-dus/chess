(ns chess.bitboard.file-rank)

(def  lookup-file  ^ints (into-array Integer/TYPE (take 64 (cycle (range 1 9)))))

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
    (mapcat #(repeat 8 %) (take 8 (iterate (partial + 8) 1)))))

(def ^:longs square-rank-row-mask-array
  "used to get a specific rank row. array has for each"
  (into-array Long/TYPE
    (map #(bit-shift-left 2r11111111 (* 8 (- % 1))) rank-squares)))
