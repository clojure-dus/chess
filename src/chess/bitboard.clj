; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards

(ns chess.bitboard)
(set! *warn-on-reflection* true)
(use 'clojure.pprint)

(def ^:const  Empty 0)
(def ^:const  WhitePawn 1)
(def ^:const  WhiteRook 2)
(def ^:const  WhiteKnight 3)
(def ^:const  WhiteBishop 4)
(def ^:const  WhiteQueen 5)
(def ^:const  WhiteKing 6)
(def ^:const  BlackPawn 7)
(def ^:const  BlackRook 8)
(def ^:const  BlackKnight 9)
(def ^:const  BlackBishop 10)
(def ^:const  BlackQueen 11)
(def ^:const  BlackKing 12)

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

(defn flatten-coord [file rank]
  (let [ file (inc file)
         rank (inc rank)]
    (- (+ (* 8 rank) file) 9)))

(comment
  (def bitvector (reduce (fn [bm x] (conj bm (expt 2 x))) [] (range 63))))

(def  lookupFile  ^ints (into-array Integer/TYPE (take 64 (cycle (range 1 9)))))

(def  lookupRank  ^ints (into-array Integer/TYPE (flatten (map (partial  repeat 8) (range 1 9)))))

(def file-rank-squares
  (map #(let [file (aget lookupFile %) rank (aget lookupRank %)] [% file rank ])(range 64)))

(def lookup-king-attacks
  "creates a lookup array of  64 squares which have bitboards
   in which knightattacks have been flaged "
  (let [result (make-array Long/TYPE 65)

        king-moves   [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]]

        all-moves  (for [[square file rank] file-rank-squares
                         [x y] king-moves
                         :let  [f (+ file x) r (+ rank y)]
                         :when (and (> f -1 ) (< f 8) (> r -1) (< r 8))]
                     [square f r])
        ](doseq [[square f r] all-moves]
                       (let [b (aget result square)
                             bit (bitvector (flatten-coord f r))]
                         (aset result square (bit-or b bit))))
         result))

(def ^ChessBoard initial-board
  (let [squares (apply
                 (partial vector-of :int)
                  (concat [WhiteRook WhiteKnight WhiteBishop WhiteQueen
                                     WhiteKing WhiteBishop WhiteKnight WhiteRook]
                                    (repeat 8 WhitePawn)
                                    (repeat 32 Empty)
                                    (repeat 8 BlackPawn)
                                    [BlackRook BlackKnight BlackBishop BlackQueen BlackKing
                                     BlackBishop BlackKnight BlackRook]))]
  (ChessBoard.
   (into-array Long/TYPE [
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000000000)     
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
    (let [bbs                  (aclone ^longs (.Bitboards  board))
          squares              (.squares board)         
          from-mask            (bitvector from-square)
          to-mask              (bitvector dest-square)
          update-mask          (bit-or from-mask  to-mask)
          piece-idx            (int (squares from-square))
          captured-idx         (int (squares dest-square))
          piece-bitboard       (aget bbs piece-idx)
          captured-bitboard    (aget bbs captured-idx); if empty uses dummy bitboard 
          squares              (assoc-in squares [from-square] Empty)
          squares              (assoc-in squares [dest-square] piece-idx)]
            (aset-long bbs piece-idx (bit-xor piece-bitboard  update-mask))
            (aset-long bbs captured-idx (bit-xor captured-bitboard to-mask))
            (let [
              whitepieces (bit-or (aget bbs WhitePawn)
                              (aget bbs WhiteRook)
                              (aget bbs WhiteKnight)
                              (aget bbs WhiteBishop)
                              (aget bbs WhiteQueen)
                              (aget bbs WhiteKing))
              blackpieces (bit-or (aget bbs BlackPawn)
                              (aget bbs BlackRook)
                              (aget bbs BlackKnight)
                              (aget bbs BlackBishop)
                              (aget bbs BlackQueen)
                              (aget bbs BlackKing))
              allpieces (bit-or whitepieces blackpieces)]
               (ChessBoard. bbs whitepieces blackpieces allpieces squares))))
   
(defn move [^ChessBoard board  from-cord to-cord]
  (let [ [from-file from-rank] from-cord
         [to-file to-rank]     to-cord
          from-pos             (flatten-coord from-file from-rank)
          to-pos               (flatten-coord to-file to-rank)
          ] (update-chessboard board from-pos to-pos)))

;(move initial-board  [0 1] [0 3])




;(compile 'chess.bitboard)

