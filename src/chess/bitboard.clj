; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards

(ns chess.bitboard)
(set! *warn-on-reflection* true)
(use 'clojure.pprint)
(import 'chess.BitOps)

(defn unsigned-shift-right[x n]
  (BitOps/unsignedShiftRight x n))

(comment
  (println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))

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

(defn white? [piece] (and (> piece Empty) (< piece BlackPawn)))

(let [bit-vec  (loop [bitmasks (vector-of :long 2r1) n 0]
    (if (= 63 n) bitmasks
        (recur  (conj bitmasks (bit-shift-left (last bitmasks) 1)) (inc n))))]
  (defn get-bitmask ^long [x] (bit-vec x)))

(defrecord ChessBoard [^longs Bitboards
                       ^long  Whitepieces
                       ^long  Blackpieces
                       ^long  Allpieces
                       squares
                       ])

(comment
  (def get-bitmask (reduce (fn [bm x] (conj bm (expt 2 x))) [] (range 63))))

(def  lookup-file  ^ints (into-array Integer/TYPE (take 64 (cycle (range 1 9)))))

(def  lookup-rank  ^ints (into-array Integer/TYPE (flatten (map (partial  repeat 8) (range 1 9)))))

(defn lookup-file-rank [square] [(aget lookup-file square) (aget lookup-rank square)])

(def file-rank-squares
 (map #(let [file (aget lookup-file %) rank (aget lookup-rank %)] [% file rank ])(range 64)))

(defn coord->square [file rank]
    (- (+ (* 8 rank) file) 9))

(defn square->coord [square]
  [( aget lookup-file square) (aget lookup-rank square)])

(let [ index-64 (into-array Byte/TYPE [
            63,  0, 58,  1, 59, 47, 53,  2,
            60, 39, 48, 27, 54, 33, 42,  3,
            61, 51, 37, 40, 49, 18, 28, 20,
            55, 30, 34, 11, 43, 14, 22,  4,
            62, 57, 46, 52, 38, 26, 32, 41,
            50, 36, 17, 19, 29, 10, 13, 21,
            56, 45, 25, 31, 35, 16,  9, 12,
            44, 24, 15,  8, 23,  7,  6,  5
                                       ])]
  (defn find-first-one-bit[^long bb]
    " return index of least significant one-bit. bb assumed to be 64 bit"
       (let [
             debruin 0x07EDD5E59A4E28C2
             term  (bit-and bb (unchecked-negate bb))
             index (unsigned-shift-right (unchecked-multiply term  debruin) 58)]
       (aget index-64 index))))

(defn bitboard->coords[^long bitboard]
  (loop [bb bitboard  res []]
    (if (= 0 bb) res
        (let [square (int(find-first-one-bit bb))
              new-bb (bit-xor bb  (get-bitmask square))
              ]
          (recur new-bb (conj res (square->coord square)))))))

(def ^longs knight-attacks-array
  "creates a lookup array of  64 squares which have bitboards
   in which knightattacks have been flaged "
  (let [result (make-array Long/TYPE 65)
        knight-moves   [[1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1] [-1 2]]

        all-moves  (for [[square file rank] file-rank-squares
                         [x y] knight-moves
                         :let  [f (+ file x) r (+ rank y)]
                         :when (and (> f 0 ) (< f 9) (> r 0) (< r 9))]
                     [square f r])
        ](doseq [[square f r] all-moves]
                       (let [b (aget result square)
                             bit (get-bitmask (coord->square f r))]
                         (aset result square (bit-or b bit))))
         result))

(def lookup-attacks
  {WhiteKnight knight-attacks-array BlackKnight knight-attacks-array })

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
          from-mask            (get-bitmask from-square)
          to-mask              (get-bitmask dest-square)
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
          from-pos             (coord->square from-file from-rank)
          to-pos               (coord->square to-file to-rank)
          ] (update-chessboard board from-pos to-pos)))

(defn possible-moves [^ChessBoard board [file rank]]
  (let [
        square (coord->square file rank)
        squares              (.squares board)
        piece                (squares square)
        fn-attack            (lookup-attacks piece)
        piece-move-bitset    (aget ^longs fn-attack square)
        not-occupied-squares (bit-not (if (white? piece)
                                        (.Whitepieces board)
                                        (.Blackpieces board)))
        moves                (bit-and piece-move-bitset not-occupied-squares)
        ]
    (bitboard->coords moves)))

(defn piece-at [^ChessBoard board [file rank]]
  (let [ square (coord->square file rank)
        squares (.squares board)
        piece   (squares square)
        ]piece))


(comment
(piece-at initial-board[2 1])
  (pprint (possible-moves  initial-board [2 1])))
