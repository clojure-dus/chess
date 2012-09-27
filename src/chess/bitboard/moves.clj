(ns chess.bitboard.moves
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.chessboard])
  (:use [chess.bitboard.piece-attacks])
  (:use [clojure.pprint]))

(defmulti find-piece-moves (fn[piece _ _]
                       (case piece
                         (:N :n):Knight
                         (:R :r) :Rook
                         (:B :b) :Bishop
                         (:Q :q) :Queen
                         (:K :k) :King
                         :P      :WhitePawn
                         :p      :BlackPawn)))

 (defmethod find-piece-moves :Knight [piece from-sq game-state]
   (let [piece-move-bitset    (aget knight-attack-array from-sq)
         not-occupied-squares (bit-not (pieces-by-turn game-state))
         moves                (bit-and piece-move-bitset not-occupied-squares)]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

 (defmethod find-piece-moves :WhitePawn [piece from-sq game-state]
   (let [moves    (aget pawn-white-move-array from-sq)
         occupied (bit-and (:allpieces game-state) moves)
         moves    (bit-xor moves occupied)
         attacks  (bit-and (:blackpieces game-state) (aget pawn-white-attack-array from-sq))
         moves    (bit-or moves attacks)]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

(defmethod find-piece-moves :BlackPawn [piece from-sq game-state]
  (let [moves    (aget pawn-black-move-array from-sq)
        occupied (bit-and (:allpieces game-state) moves)
        moves    (bit-xor moves occupied)
        attacks  (bit-and (:whitepieces game-state) (aget pawn-black-attack-array from-sq))
        moves    (bit-or moves attacks)]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

 (defmethod find-piece-moves :King [piece from-sq game-state]
   (let [moves    (aget king-attack-array from-sq)
           ; todo castle move
         ]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

; for now just rank movement
(defmethod find-piece-moves :Rook [piece from-sq game-state]
  (let [allpieces            (:allpieces game-state)
        row                  (bit-and allpieces (aget square-rank-row-mask-array from-sq))
        pieces-row           (bit-shift-right row (aget rank-shift-array from-sq))
         _ (print-bitmap pieces-row :R)

        moves                (aget rank-attack-array from-sq pieces-row)
        not-occupied-squares (bit-not (pieces-by-turn game-state))
        moves                (bit-and moves not-occupied-squares)]
         (for-bitmap [dest-pos moves]
           [piece from-sq dest-pos])))


(defn possible-moves [game-state]
  (let [squares  (:board game-state)
        pieces   (pieces-by-turn game-state)]
    (for-bitmap [from-sq pieces]
      (find-piece-moves (squares from-sq) from-sq game-state))))

(comment
  (print-board  (read-fen "8/8/8/8/8/8/8/R7 w KQkq - 0 1"))

  (def t  (read-fen "8/8/8/8/8/8/1R1PPPPP/8 w KQkq - 0 1"))

  (print-board t)

  (possible-moves t))
