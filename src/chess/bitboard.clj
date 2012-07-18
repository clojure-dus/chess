; Trying to use bitboards as an alternative boardrepresentation
; Just  to implement some of Marcel's  functionality to get a feeling about the cons and pros
; off such an approach with clojure.
; Some resources
; http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/index.html
; http://chessprogramming.wikispaces.com/Bitboards
(ns bitboard)
(set! *warn-on-reflection* true)
(use 'clojure.pprint)

(defrecord ChessBoard [^long  WhitePawns
                       ^long  WhiteRooks
                       ^long  WhiteKnights
                       ^long  WhiteBishops
                       ^long  WhiteQueens
                       ^long  WhiteKing

                       ^long  BlackPawns
                       ^long  BlackRooks
                       ^long  BlackKnights
                       ^long  BlackBishops
                       ^long  BlackQueens
                       ^long  BlackKing

                       ^long  WhitePieces
                       ^long  BlackPieces
                       ^long  AllPieces]

                       ;rochade
                       ;turn
                       )

(def ^ChessBoard initial-board   (ChessBoard.
                                  2r0000000000000000000000000000000000000000000000001111111100000000
                                  2r0000000000000000000000000000000000000000000000000000000010000001
                                  2r0000000000000000000000000000000000000000000000000000000001000010
                                  2r0000000000000000000000000000000000000000000000000000000000100100
                                  2r0000000000000000000000000000000000000000000000000000000000010000
                                  2r0000000000000000000000000000000000000000000000000000000000001000

                                  2r0000000011111111000000000000000000000000000000000000000000000000
                                  2r1000000100000000000000000000000000000000000000000000000000000000
                                  2r0100001000000000000000000000000000000000000000000000000000000000
                                  2r0010010000000000000000000000000000000000000000000000000000000000
                                  2r0000100000000000000000000000000000000000000000000000000000000000
                                  2r0001000000000000000000000000000000000000000000000000000000000000

                                  2r0000000000000000000000000000000000000000000000001111111111111111
                                  2r1111111111111111000000000000000000000000000000000000000000000000
                                  2r1111111111111111000000000000000000000000000000001111111111111111

                                ;  #{:K :Q :k :q}
                                ;  :w
                                  ))

(defmacro create-update-bitboard-fn[]
  "In order to do as much work at compile-time as possible builds a big condp
with one ctor per chess piece bitboard."
   `(defn ~'update-bitboard
      [~(with-meta 'board {:tag 'ChessBoard}) ~'piece ~(with-meta 'mask {:tag 'long})]
      (cond
        ~@(let [white-bitboards
                  ['WhitePawns 'WhiteRooks 'WhiteKnights 'WhiteBishops 'WhiteQueens 'WhiteKing]

                black-bitboards
                  ['BlackPawns 'BlackRooks 'BlackKnights 'BlackBishops 'BlackQueens 'BlackKing]

                all-bitboards
                  (concat white-bitboards black-bitboards)

                board-value
                  (fn [name] (list '. 'board  name))

                ctor-expr
                  (fn [piece]
                    (let [fn-update-value (fn [field]
                                   (if (= field piece)
                                        (list 'bit-xor 'mask (board-value field))
                                        (board-value field)))

                          values (map fn-update-value all-bitboards)]
                                   (list 'let (vector 'white-pieces
                                                 (cons 'bit-or
                                                      (map fn-update-value  white-bitboards))
                                                       'black-pieces
                                                         (cons 'bit-or
                                                            (map fn-update-value black-bitboards))
                                                       'all-pieces
                                                         (list 'bit-or 'white-pieces 'black-pieces))
                                    (cons '->ChessBoard
                                      (concat values ['white-pieces 'black-pieces 'all-pieces])))))
               ]
            (apply concat ( for [piece all-bitboards]
                            [ `(~'= ~'piece '~piece) (ctor-expr piece)]))))))

(create-update-bitboard-fn)

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

(defn move [^ChessBoard board piece from-cord to-cord]
  (let [ [from-file from-rank] from-cord
         [to-file to-rank]     to-cord
          from-pos             (flatten-coord from-file from-rank)
          to-pos               (flatten-coord to-file to-rank)
          from-mask            (bitvector from-pos)
          update-mask          (bit-or from-mask  (bitvector to-pos))
          ] (update-bitboard board piece update-mask)))

(move initial-board 'WhitePawns [0 1] [0 3])
