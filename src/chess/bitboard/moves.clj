(ns chess.bitboard.moves
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.chessboard])
  (:use [clojure.pprint]))


(defn move->coord [move]
  (let [from  (bit-and move 0x0000003f)
        dest  (bit-and (bit-shift-right move 6)  0x0000003f)
        piece (bit-and(bit-shift-right move 12) 0x0000000f)
        ]
    {:piece piece :from (square->coord from) :to (square->coord dest)}))


(defn make-move ^long [ ^long from ^long dest ^long piece]
  (let[
       move 0
       move (bit-and  0x0000003f from)

       move (bit-and 0xfffff03f move)
       move (bit-or  move (bit-shift-left (bit-and 0x0000003f dest) 6))

       move (bit-and  0xffff0fff move)
       move (bit-or  move (bit-shift-left (bit-and 0x0000000f piece) 12))]
    move))

(defn possible-moves [^chess.bitboard.chessboard.ChessBoard board]
  (let [result          (transient [])
        squares         (.squares board)
        wanted-pieces   (.Whitepieces board)
        ]
        (bitmap-seq [from wanted-pieces]
             (let[
                  piece                (aget squares from)
                  piece-attack-arr     (lookup-attacks piece)
                  piece-move-bitset    (aget  piece-attack-arr from)
                  not-occupied-squares (bit-not wanted-pieces)
                  moves                (bit-and piece-move-bitset not-occupied-squares)
                  ]
               (bitmap-seq [dest moves]
                           (do          (print "from : " from  ", dest : " dest)
                             (conj! result (make-move from dest piece))))))
        (persistent! result)))


(defn print-moves [moves]
  (pprint (map move->coord moves)))
