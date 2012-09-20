(ns chess.bitboard.moves
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.chessboard])
  (:use [clojure.pprint]))


(defn possible-moves [game-state]
  (let [result          (transient [])
        squares         (:board game-state)
        pieces          (if (= (game-state :turn) :w)
                          (:whitepieces game-state)
                          (:blackpieces game-state))
        ]
        (bitmap-seq [from pieces]
             (let[
                  piece                (squares nth from)
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
