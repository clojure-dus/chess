(ns chess.bitboard.moves
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.chessboard])
  (:use [chess.bitboard.piece-attacks])
  (:use [clojure.pprint]))


(defmulti find-moves (fn[piece from-pos game-state]
                       (case piece
                         (:N :n) :Knight
                         (:R :r) :Rook
                         (:B :b) :Bishop
                         (:Q :q) :Queen
                         (:K :k) :King
                         (:P :p) :Pawn)))

(defmethod find-moves :Knight [piece from-pos game-state]
  (let [piece-move-bitset    (aget knight-attacks-array from-pos)
        not-occupied-squares (bit-not (pieces-by-turn game-state))
        moves                (bit-and piece-move-bitset not-occupied-squares)]
              (for-bitmap [dest-pos moves]
                [piece from-pos dest-pos])))


(defn possible-moves [game-state]
  (let [squares  (:board game-state)
        pieces   (pieces-by-turn game-state)]
    (for-bitmap [from pieces]
       (find-moves (squares from) from game-state))))


(def two-lonely-knight (read-fen "8/8/8/8/8/8/8/1N4N1 w KQkq - 0 1"))
(comment
  (print-board two-lonely-knight))

(possible-moves two-lonely-knight)
