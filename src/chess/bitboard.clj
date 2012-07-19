; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards
(ns chess.bitboard)
(set! *warn-on-reflection* true)
(use 'clojure.pprint)

(def ^:const WhitePawnsIdx 0)
(def ^:const WhiteRooksIdx 1)
(def ^:const WhiteKnightsIdx 2)
(def ^:const WhiteBishopsIdx 3)
(def ^:const WhiteQueensIdx 4)
(def ^:const WhiteKingIdx 5)
(def ^:const BlackPawnsIdx 6)
(def ^:const BlackRooksIdx 7)
(def ^:const BlackKnightsIdx 8)
(def ^:const BlackBishopsIdx 9)
(def ^:const BlackQueensIdx 10)
(def ^:const BlackKingIdx 11)

(defrecord ChessBoard [^longs Bitboards
                       ^long  Whitepieces
                       ^long  Blackpieces
                       ^long  Allpieces])

(def ^ChessBoard initial-board
          (ChessBoard.
             (into-array Long/TYPE [
               (unchecked-long 2r0000000000000000000000000000000000000000000000001111111100000000)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000010000001)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000001000010)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000100100)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000010000)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000001000)

               (unchecked-long 2r0000000011111111000000000000000000000000000000000000000000000000)
               (unchecked-long 2r1000000100000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0100001000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0010010000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0000100000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0001000000000000000000000000000000000000000000000000000000000000)])

              2r0000000000000000000000000000000000000000000000001111111111111111
              2r1111111111111111000000000000000000000000000000000000000000000000
              2r1111111111111111000000000000000000000000000000001111111111111111))

  (defn update-bitboard [^ChessBoard board  piece ^long mask]
    (let [bbs (aclone ^longs (.Bitboards  board))
          old (aget bbs piece)
          _ (aset-long bbs piece (bit-xor old mask))
          whitepieces (bit-or (aget bbs WhitePawnsIdx)
                              (aget bbs WhiteRooksIdx)
                              (aget bbs WhiteKnightsIdx)
                              (aget bbs WhiteBishopsIdx)
                              (aget bbs WhiteQueensIdx)
                              (aget bbs WhiteKingIdx))

          blackpieces (bit-or (aget bbs BlackPawnsIdx)
                              (aget bbs BlackRooksIdx)
                              (aget bbs BlackKnightsIdx)
                              (aget bbs BlackBishopsIdx)
                              (aget bbs BlackQueensIdx)
                              (aget bbs BlackKingIdx))

          allpieces (bit-or whitepieces blackpieces)
          ]
      (ChessBoard. bbs whitepieces blackpieces allpieces)))

(defn flatten-coord [file rank]
  (let [ file (inc file)
         rank (inc rank)]
    (- (+ (* 8 rank) file) 9)))

(comment
  (def bitvector (reduce (fn [bm x] (conj bm (expt 2 x))) [] (range 63))))

(let [bit-vec  (loop [bitmasks (vector-of :long 2r1) n 0]
    (if (= 63 n) bitmasks
        (recur  (conj bitmasks (bit-shift-left (last bitmasks) 1)) (inc n))))]
  (defn bitvector ^long [x] (bit-vec x)))

(defn move [^ChessBoard board pieceindex from-cord to-cord]
  (let [ [from-file from-rank] from-cord
         [to-file to-rank]     to-cord
          from-pos             (flatten-coord from-file from-rank)
          to-pos               (flatten-coord to-file to-rank)
          from-mask            (bitvector from-pos)
          update-mask          (bit-or from-mask  (bitvector to-pos))
          ] (update-bitboard board pieceindex update-mask)))

(move initial-board WhitePawnsIdx [0 1] [0 3])




;(compile 'chess.bitboard)
