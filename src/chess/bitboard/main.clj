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
  )
