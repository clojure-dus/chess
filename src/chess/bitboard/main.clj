; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards
(ns chess.bitboard.main
  (:use [chess.bitboard.piece-attacks])
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.chessboard])
  (:use [chess.bitboard.moves])
  (:use [chess.fen])
  (:import (chess.bitboard.chessboard ChessUpdate ChessBoard))
  )

(defn board->bitboard [{:keys [board]}]
  (let [squares (flatten board)
        squares (map-indexed vector squares)
        update-fn (fn [^ChessUpdate board [^int pos piece]]
                    (.setPiece board pos ^int (keyword-piece-map piece)))
        ] (reduce update-fn empty-board  squares)))


(board->bitboard (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
