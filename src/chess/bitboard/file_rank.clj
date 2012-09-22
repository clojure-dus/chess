(ns chess.bitboard.file-rank)

(def  lookup-file  ^ints (into-array Integer/TYPE (take 64 (cycle (range 1 9)))))

(def  lookup-rank  ^ints (into-array Integer/TYPE (flatten (map (partial  repeat 8) (range 1 9)))))

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
