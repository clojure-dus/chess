(ns chess.bitboard.moves
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.chessboard])
   (:use [chess.bitboard.piece-attacks])

  (:use [clojure.pprint]))


(defmulti find-moves (fn[piece from-pos game-state] piece))

(defmethod find-moves :N [piece from-pos game-state]
  (let [piece-move-bitset    (aget knight-attacks-array from-pos)
                  not-occupied-squares (bit-not (:whitepieces game-state))
                  moves                (bit-and piece-move-bitset not-occupied-squares)]
              (for-bitmap [dest-pos moves]
                [piece from-pos dest-pos])))


(defn possible-moves [game-state]
  (let [squares         (:board game-state)
        pieces          (if (= (game-state :turn) :w)
                          (:whitepieces game-state)
                          (:blackpieces game-state))
        ]
    (for-bitmap [from pieces]
             (find-moves :N  from game-state))))

(comment
  (def a-lonely-knight (read-fen "8/8/8/8/8/8/8/6N1 w KQkq - 0 1"))
  (print-board a-lonely-knight)
  (possible-moves a-knight))
