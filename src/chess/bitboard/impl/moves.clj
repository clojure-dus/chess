(ns chess.bitboard.impl.moves
  (:use [chess.bitboard.impl.file-rank])
  (:use [chess.bitboard.impl.bitoperations])
  (:use [chess.bitboard.impl.chessboard])
  (:use [chess.bitboard.impl.piece-attacks])
  (:use [clojure.pprint]))
(comemnt (set! *warn-on-reflection* true))
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
   (let [piece-move-bitset    (aget ^longs knight-attack-array from-sq)
         not-occupied-squares (bit-not (pieces-by-turn game-state))
         moves                (bit-and piece-move-bitset not-occupied-squares)]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

 (defmethod find-piece-moves :WhitePawn [piece from-sq game-state]
   (let [moves          (aget ^longs pawn-white-move-array from-sq)
         all-pieces     (:allpieces game-state)
         occupied       (bit-and all-pieces moves)
         moves          (bit-xor moves occupied)

         double-moves   (aget ^longs pawn-white-double-move-array from-sq)
         occupied-4-row (bit-and all-pieces double-moves)
         occupied-3-row (bit-and (bit-shift-left all-pieces 8) double-moves)
         double-moves   (bit-xor double-moves (bit-or occupied-3-row occupied-4-row))

         attacks  (bit-and (:blackpieces game-state) (aget ^longs pawn-white-attack-array from-sq))
         moves    (bit-or moves double-moves attacks)
         ]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))


(defmethod find-piece-moves :BlackPawn [piece from-sq game-state]
  (let [moves    (aget ^longs pawn-black-move-array from-sq)
        all-pieces     (:allpieces game-state)
        occupied (bit-and all-pieces moves)
        moves    (bit-xor moves occupied)

        double-moves   (aget ^longs pawn-black-double-move-array from-sq)
        occupied-5-row (bit-and all-pieces double-moves)
        occupied-6-row (bit-and (bit-shift-right  all-pieces 8) double-moves)
        double-moves   (bit-xor double-moves (bit-or occupied-5-row occupied-6-row))

        attacks  (bit-and (:whitepieces game-state) (aget ^longs pawn-black-attack-array from-sq))
        moves    (bit-or moves double-moves attacks)]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

 (defmethod find-piece-moves :King [piece from-sq game-state]
   (let [moves              (aget ^longs king-attack-array from-sq)
         not-occupied       (bit-not (pieces-by-turn game-state))
         moves              (bit-and moves not-occupied)
         ; todo castle move
         ]
     (for-bitmap [dest-pos moves]
       [piece from-sq dest-pos])))

(defmethod find-piece-moves :Rook [piece from-sq game-state]
  (let [allpieces          (:allpieces game-state)
        not-occupied       (bit-not (pieces-by-turn game-state))

        occupied-row       (bit-and allpieces (aget masks-row from-sq))
        occupied-mask      (unsigned-shift-right occupied-row
                                                 (inc (aget rank-shift-array from-sq)))
        slide-moves-rank   (bit-and (aget ^longs attack-array-ranks from-sq occupied-mask)
                                    not-occupied)

        occupied-column    (bit-and allpieces (aget ^longs masks-column from-sq))
        occupied-mask      (shift-rank-to-bottom occupied-column from-sq)

        slide-moves-file   (bit-and (aget ^longs attack-array-files from-sq occupied-mask) not-occupied)

        slide-moves        (bit-or slide-moves-rank slide-moves-file) ]

    (for-bitmap [dest-pos slide-moves]
      [piece from-sq dest-pos])))

(defmethod find-piece-moves :Bishop [piece from-sq game-state]
  (let [allpieces          (:allpieces game-state)
        not-occupied       (bit-not (pieces-by-turn game-state))

        occupied-diagonal  (bit-and allpieces (aget ^longs masks-diagonal-a1h8 from-sq))

        occupied-mask      (shift-diagonal-a1h8-to-bottom occupied-diagonal from-sq)
        slide-diagonal-a1  (bit-and (aget ^longs attack-array-diagonal-a1h8
                                            from-sq occupied-mask) not-occupied)
        occupied-diagonal  (bit-and allpieces (aget ^longs masks-diagonal-a8h1 from-sq))
        occupied-mask      (shift-diagonal-a8h1-to-bottom occupied-diagonal from-sq)
        slide-diagonal-a8  (bit-and (aget ^longs attack-array-diagonal-a8h1
                                          from-sq occupied-mask) not-occupied)

        slide-moves        (bit-or slide-diagonal-a1 slide-diagonal-a8)]
         (for-bitmap [dest-pos slide-moves]
           [piece from-sq dest-pos])))

(defmethod find-piece-moves :Queen  [piece from-sq game-state]
  (concat (find-piece-moves  :r from-sq game-state)
           (find-piece-moves :b from-sq game-state)))


(defn generate-moves [game-state]
  (let [squares  (:board game-state)
        pieces   (pieces-by-turn game-state)]
    (apply concat (for-bitmap [from-sq pieces]
                    (find-piece-moves (squares from-sq) from-sq game-state)))))

(defn print-generate-moves [game-state]
  (print-board
    (create-board-fn
        (map (fn [[p x y]] [p y])
             (generate-moves game-state))) ))


(comment (print-generate-moves  (read-fen "8/8/8/8/8/1B6/8/8 w KQkq - 0 1")))
