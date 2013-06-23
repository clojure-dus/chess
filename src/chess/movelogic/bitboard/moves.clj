(ns chess.movelogic.bitboard.moves
  (:use [chess.movelogic.bitboard bitoperations file-rank chessboard piece-attacks])
  (:use [clojure.pprint]))

(require '[clojure.core.reducers :as r])


(comment (set! *warn-on-reflection* true))

(defn attacked-by-black? [mask-sq game-state]
  "looks at squares set in mask-sq and returns true if black attacks any of these squares"
  (some-bitmap  (fn [sq]
                  (or (bit-and? (:p game-state) (aget ^longs pawn-white-attack-array sq))
                      (bit-and? (:k game-state) (aget ^longs king-attack-array  sq))
                      (bit-and? (:n game-state) (aget ^longs knight-attack-array sq))

                      (bit-and? (bit-or (:r game-state) (:q game-state))
                                (get-attack-rank game-state sq))
                      (bit-and? (bit-or (:r game-state) (:q game-state))
                                (get-attack-file game-state sq))

                      (bit-and? (bit-or (:b game-state) (:q game-state))
                                (get-attack-diagonal-a1h8 game-state sq))
                      (bit-and? (bit-or (:b game-state) (:q game-state))
                                (get-attack-diagonal-a8h1 game-state sq)))) mask-sq))

(defn attacked-by-white? [mask-sq game-state]
  "looks at squares set in mask-sq and returns true if white attacks any of these squares"
  (some-bitmap  (fn [sq]
                  (or (bit-and? (:P game-state) (aget ^longs pawn-black-attack-array sq))
                      (bit-and? (:K game-state) (aget ^longs king-attack-array  sq))
                      (bit-and? (:N game-state) (aget ^longs knight-attack-array sq))

                      (bit-and? (bit-or (:R game-state) (:Q game-state))
                                (get-attack-rank game-state sq))
                      (bit-and? (bit-or (:R game-state) (:Q game-state))
                                (get-attack-file game-state sq))

                      (bit-and? (bit-or (:B game-state) (:Q game-state))
                                (get-attack-diagonal-a1h8 game-state sq))
                      (bit-and? (bit-or (:B game-state) (:Q game-state))
                                (get-attack-diagonal-a8h1 game-state sq)))) mask-sq))

(defn check? [game-state]
 (if (= :w (:turn game-state))
      (attacked-by-black? (:K game-state) game-state)
      (attacked-by-white? (:k game-state) game-state)))


(defmulti find-piece-moves (fn[piece _ _]
                       (case piece
                         (:N :n) :Knight
                         (:R :r) :Rook
                         (:B :b) :Bishop
                         (:Q :q) :Queen
                         :K      :WhiteKing
                         :k      :BlackKing
                         :P      :WhitePawn
                         :p      :BlackPawn)))

 (defmethod find-piece-moves :Knight [piece game-state from-sq]
   (let [piece-move-bitset    (aget ^longs knight-attack-array from-sq)
         not-occupied-squares (bit-not (pieces-by-turn game-state))]
     (bit-and piece-move-bitset not-occupied-squares)))

 (defmethod find-piece-moves :WhitePawn [piece game-state from-sq]
   (let [moves          (aget ^longs pawn-white-move-array from-sq)
         all-pieces     (:allpieces game-state)
         occupied       (bit-and all-pieces moves)
         moves          (bit-xor moves occupied)

         double-moves   (aget ^longs pawn-white-double-move-array from-sq)
         occupied-4-row (bit-and all-pieces double-moves)
         occupied-3-row (bit-and (bit-shift-left all-pieces 8) double-moves)
         double-moves   (bit-xor double-moves (bit-or occupied-3-row occupied-4-row))

         attacks  (bit-and (:blackpieces game-state)
                           (aget ^longs pawn-white-attack-array from-sq))]
     (bit-or moves double-moves attacks)))

(defmethod find-piece-moves :BlackPawn [piece game-state from-sq]
  (let [moves          (aget ^longs pawn-black-move-array from-sq)
        all-pieces     (:allpieces game-state)
        occupied       (bit-and all-pieces moves)
        moves          (bit-xor moves occupied)

        double-moves   (aget ^longs pawn-black-double-move-array from-sq)
        occupied-5-row (bit-and all-pieces double-moves)
        occupied-6-row (bit-and (bit-shift-right  all-pieces 8) double-moves)
        double-moves   (bit-xor double-moves (bit-or occupied-5-row occupied-6-row))

        attacks        (bit-and (:whitepieces game-state)
                                (aget ^longs pawn-black-attack-array from-sq))]
    (bit-or moves double-moves attacks)))

(defn  ^Long get-rochade-moves  [game-state kind]
  (let [rochade  (:rochade game-state )
        occupied (pieces-by-turn game-state)]
    (condp = kind
     :K (if (and (:K rochade)
                 (not (bit-and? mask-rochade-white-king occupied))
                 (not (attacked-by-black? mask-rochade-white-king game-state)))
            move-rochade-white-king 0)
     :Q (if (and (:Q rochade)
                  (not (bit-and? mask-rochade-white-queen occupied))
                 (not (attacked-by-black? mask-rochade-white-queen game-state)))
           move-rochade-white-queen 0)
     :k (if (and (:k rochade)
                 (not (bit-and? mask-rochade-black-king occupied))
                 (not (attacked-by-white? mask-rochade-black-king game-state)))
           move-rochade-black-king 0)
     :q (if (and (:q rochade)
                 (not (bit-and? mask-rochade-black-queen occupied))
                 (not (attacked-by-white? mask-rochade-black-queen game-state)))
           move-rochade-black-queen 0))))

 (defmethod find-piece-moves :WhiteKing [piece game-state from-sq]
   (let [moves             (aget ^longs king-attack-array from-sq)
         occupied           (pieces-by-turn game-state)
         not-occupied       (bit-not occupied)
         moves              (bit-and moves not-occupied)]
           (bit-or moves
                   (get-rochade-moves game-state :K)
                   (get-rochade-moves game-state :Q))))

(defmethod find-piece-moves :BlackKing [piece game-state from-sq]
   (let [moves              (aget ^longs king-attack-array from-sq)
         ^long occupied     (pieces-by-turn game-state)
         not-occupied       (bit-not occupied)
         moves              (bit-and moves not-occupied)]
            (bit-or moves
                   (get-rochade-moves game-state :k)
                   (get-rochade-moves game-state :q))))

(defmethod find-piece-moves :Rook [piece game-state from-sq]
  (let [not-occupied       (bit-not (pieces-by-turn game-state))
        slide-moves-rank   (bit-and (get-attack-rank game-state from-sq) not-occupied)
        slide-moves-file   (bit-and (get-attack-file  game-state from-sq) not-occupied)
        slide-moves        (bit-or slide-moves-rank slide-moves-file)]
    slide-moves))

(defmethod find-piece-moves :Bishop [piece game-state from-sq]
  (let [not-occupied       (bit-not (pieces-by-turn game-state))
        slide-diagonal-a1  (bit-and (get-attack-diagonal-a1h8 game-state from-sq) not-occupied)
        slide-diagonal-a8  (bit-and (get-attack-diagonal-a8h1 game-state from-sq) not-occupied)]
    (bit-or slide-diagonal-a1 slide-diagonal-a8)))

(defmethod find-piece-moves :Queen  [piece game-state from-sq]
  (let [not-occupied       (bit-not (pieces-by-turn game-state))
        slide-moves-rank   (bit-and (get-attack-rank game-state from-sq) not-occupied)
        slide-moves-file   (bit-and (get-attack-file  game-state from-sq) not-occupied)
        slide-moves-rook   (bit-or slide-moves-rank slide-moves-file)
        slide-diagonal-a1  (bit-and (get-attack-diagonal-a1h8 game-state from-sq) not-occupied)
        slide-diagonal-a8  (bit-and (get-attack-diagonal-a8h1 game-state from-sq) not-occupied)]
    (bit-or slide-diagonal-a1 slide-diagonal-a8 slide-moves-rook)))


(defn create-move [piece from-sq dest-pos]
  (condp = piece
    :P (if (< dest-pos 56)
         [piece from-sq dest-pos]
         [[piece from-sq dest-pos :Q] ; means last row so pawn gets promoted
          [piece from-sq dest-pos :R]
          [piece from-sq dest-pos :B]
          [piece from-sq dest-pos :N]])
    :p (if (> dest-pos 7)
         [piece from-sq dest-pos]
         [[piece from-sq dest-pos :q] ; means last row so pawn gets promoted
          [piece from-sq dest-pos :r]
          [piece from-sq dest-pos :b]
          [piece from-sq dest-pos :n]])
    [piece from-sq dest-pos]))

(defn move-parser [piece from-square]
  (fn[bitboard] (r/map (partial create-move piece from-square) bitboard)))

(defn find-moves[game-state]
  (fn[square]
    (let [piece            (get (:board game-state)  square)
          get-piece-moves  (partial find-piece-moves piece game-state)
          create-move      (move-parser piece square)
          transform         (comp create-move  get-piece-moves)]
         (transform square))))



(defn possible-moves [game-state pieces]
  (into [] (r/mapcat  (find-moves game-state) pieces)))


(defn generate-moves [game-state]
  (possible-moves game-state  (pieces-by-turn game-state)))


(defn print-generate-moves [game-state]
  (print-board
   (create-board-fn
    (map (fn [[p x y]] [p y])
         (generate-moves game-state) ))))
