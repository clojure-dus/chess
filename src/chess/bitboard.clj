; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards
(ns chess.bitboard)
(set! *warn-on-reflection* true)
(use 'clojure.pprint)

(def ^:const NoIdx -1)
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

(let [bit-vec  (loop [bitmasks (vector-of :long 2r1) n 0]
    (if (= 63 n) bitmasks
        (recur  (conj bitmasks (bit-shift-left (last bitmasks) 1)) (inc n))))]
  (defn bitvector ^long [x] (bit-vec x)))

(defrecord ChessBoard [^longs Bitboards
                       ^long  Whitepieces
                       ^long  Blackpieces
                       ^long  Allpieces
                       squares
                       ])

(def ^ChessBoard initial-board
  (let [squares (apply (partial vector-of :int)
                           (concat '(1 2 3 4 5 3 2 1)
                           (repeat 8 WhitePawnsIdx)
                           (repeat 32 NoIdx)
                           (repeat 8 BlackPawnsIdx)
                           '(7 8 9 10 11 9 8 7)))]
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

               (unchecked-long 2r0000000000000000000000000000000000000000000000001111111111111111)
               (unchecked-long 2r1111111111111111000000000000000000000000000000000000000000000000)
               (unchecked-long 2r1111111111111111000000000000000000000000000000001111111111111111)

               squares)))

  (defn update-chessboard [^ChessBoard board  ^long from-square ^long dest-square]
    (let [bbs  (aclone ^longs (.Bitboards  board))
          squares (.squares board)
         
          from-mask            (bitvector from-square)
          to-mask              (bitvector dest-square)
          update-mask          (bit-or from-mask  to-mask)
          piece-idx            (squares from-square)
          captured-piece-idx   (squares dest-square)
          squares              (assoc-in squares [from-square] NoIdx)
          squares              (assoc-in squares [dest-square] piece-idx)]
            (aset-long bbs piece-idx (bit-xor (aget bbs piece-idx) update-mask))
            (when (> -1 captured-piece-idx)
               (aset-long bbs captured-piece-idx
                      (bit-xor (aget bbs captured-piece-idx) to-mask)))
            (let [
          
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

              allpieces (bit-or whitepieces blackpieces)]
        (ChessBoard. bbs whitepieces blackpieces allpieces squares))))
   
(defn flatten-coord [file rank]
  (let [ file (inc file)
         rank (inc rank)]
    (- (+ (* 8 rank) file) 9)))

(comment
  (def bitvector (reduce (fn [bm x] (conj bm (expt 2 x))) [] (range 63))))

(defn move [^ChessBoard board pieceindex from-cord to-cord]
  (let [ [from-file from-rank] from-cord
         [to-file to-rank]     to-cord
          from-pos             (flatten-coord from-file from-rank)
          to-pos               (flatten-coord to-file to-rank)
          0
          ] (update-chessboard board from-pos to-pos)))

(move initial-board WhitePawnsIdx [0 1] [0 3])



;(compile 'chess.bitboard)
