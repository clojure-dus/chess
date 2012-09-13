(ns chess.bitboard.chessboard
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.piece-attacks]))


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

(def piece-char-map  {Empty       " "
                      WhitePawn   "P"
                      WhiteRook   "R"
                      WhiteKnight "N"
                      WhiteBishop "B"
                      WhiteQueen  "Q"
                      WhiteKing   "K"
                      BlackPawn   "p"
                      BlackRook   "r"
                      BlackKnight "n"
                      BlackBishop "b"
                      BlackQueen  "q"
                      BlackKing   "k"
                      })

(def keyword-piece-map  { :_ Empty
                          :P WhitePawn
                          :R WhiteRook
                          :N WhiteKnight
                          :B WhiteBishop
                          :Q WhiteQueen
                          :K WhiteKing
                          :p BlackPawn
                          :r BlackRook
                          :n BlackKnight
                          :b BlackBishop
                          :q BlackQueen
                          :k BlackKing
                      })

(def lookup-attacks
  {WhiteKnight knight-attacks-array BlackKnight knight-attacks-array })

(defmacro build-whitepieces [bitboards]
`(bit-or (aget ~bitboards WhitePawn)
         (aget ~bitboards WhiteRook)
         (aget ~bitboards WhiteKnight)
         (aget ~bitboards WhiteBishop)
         (aget ~bitboards WhiteQueen)
         (aget ~bitboards WhiteKing)))

(defmacro build-blackpieces[bitboards]
`(bit-or (aget ~bitboards BlackPawn)
         (aget ~bitboards BlackRook)
         (aget ~bitboards BlackKnight)
         (aget ~bitboards BlackBishop)
         (aget ~bitboards BlackQueen)
         (aget ~bitboards BlackKing)))

(defmacro build-allpieces[bitboards]
  `(bit-or  (build-whitepieces ~bitboards) (build-blackpieces ~bitboards)))

(definterface ChessUpdate
  (^Object movePiece [ ^int from ^int dest])
  (^Object setPiece  [  ^int pos ^int piece]))


(deftype ChessBoard [  ^longs Bitboards
                       ^long  Whitepieces
                       ^long  Blackpieces
                       ^long  Allpieces
                       ^ints  squares]
  Object
  (toString
    [this]
    (let [
         line "  +--+-+-+-+-+-+-+--+\n"
         abc  "    A B C D E F G H\n"
         rows (map piece-char-map (.squares this))
         rows (map (fn[x] ( str x " ")) rows)
         rows (partition 8 rows)
         rows (map (fn[x file] (vec(cons (str file " | ") x))) rows (range 1 9))
         rows (map (fn [x] (conj  x  "|\n")) rows)
         rows (apply str (flatten rows))
      ]
      (str abc line rows line abc)))
  ChessUpdate
  ( movePiece [^ChessBoard this ^int from  ^int dest]
    (let [bbs                  (aclone ^longs (.Bitboards this))
          squares              (aclone ^ints  (.squares this))
          from-mask            (square->bit from)
          to-mask              (square->bit dest)
          update-mask          (bit-or from-mask  to-mask)
          piece                (aget squares from)
          captured-piece       (aget squares dest)
          piece-bitboard       (aget bbs piece)
          captured-bitboard    (aget bbs captured-piece); if empty uses dummy bitboard
         ]
            (aset-int squares dest piece)
            (aset-int squares from Empty)
            (aset-long bbs piece (bit-xor piece-bitboard  update-mask))
            (aset-long bbs captured-piece (bit-xor captured-bitboard to-mask))
            (ChessBoard. bbs
                         (build-whitepieces bbs)
                         (build-blackpieces bbs)
                         (build-allpieces bbs)
                          squares)))

(setPiece [^ChessBoard this ^int square ^int piece]
     (let [
           bbs                 (.Bitboards this)
           update-mask         (square->bit square)
           piece-bitboard       (aget bbs piece)
           ]
            (aset-int squares square piece)
            (aset-long  bbs piece (bit-or piece-bitboard  update-mask))
            this)))

  (defn print-chessboard [board writer]
    (.write writer (str board)))
(comment
  (.addMethod simple-dispatch ChessBoard (fn [board]
                                           (print-chessboard board *out*)))
)
  (defmethod print-method ChessBoard [board writer]
    (print-chessboard board writer))


(def ^ChessBoard initial-board
  (let [
        squares (into-array Integer/TYPE [
                                          BlackRook,BlackKnight,BlackBishop,BlackQueen,
                                          BlackKing,BlackBishop,BlackKnight,BlackRook,
                                          7,7,7,7,7,7,7,7,
                                          0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0,
                                          1,1,1,1,1,1,1,1,
                                          WhiteRook,WhiteKnight,WhiteBishop,WhiteQueen,
                                          WhiteKing,WhiteBishop,WhiteKnight,WhiteRook
                                          ])]
  (ChessBoard.
   (into-array Long/TYPE [
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000000000)


               (unchecked-long 2r0000000011111111000000000000000000000000000000000000000000000000)
               (unchecked-long 2r1000000100000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0100001000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0010010000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0000100000000000000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0001000000000000000000000000000000000000000000000000000000000000)

               (unchecked-long 2r0000000000000000000000000000000000000000000000001111111100000000)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000010000001)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000001000010)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000100100)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000010000)
               (unchecked-long 2r0000000000000000000000000000000000000000000000000000000000001000)
               ])
               (unchecked-long 2r1111111111111111000000000000000000000000000000000000000000000000)
               (unchecked-long 2r0000000000000000000000000000000000000000000000001111111111111111)
               (unchecked-long 2r1111111111111111000000000000000000000000000000001111111111111111)

               squares
               )))

(def ^ChessBoard empty-board
  (ChessBoard.
   (make-array Long/TYPE 13)
               (long 0)
               (long 0)
               (long 0)
               (make-array Integer/TYPE 64)))
