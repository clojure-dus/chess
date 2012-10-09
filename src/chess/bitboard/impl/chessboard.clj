(ns chess.bitboard.impl.chessboard
  (:use [chess.bitboard.impl.bitoperations])
  (:use [chess.bitboard.impl.file-rank])
  (:use [chess.bitboard.impl.piece-attacks])
  (:use [chess.fen :rename {read-fen other-impl-read-fen}]))

(def empty-board
  {:board  (vec (repeat 64 :_))
   :turn :w
   :rochade #{:K :Q :k :q }
   :r 0 :n 0 :b 0 :q 0 :k 0 :p 0
   :R 0 :N 0 :B 0 :Q 0 :K 0 :P 0
   :_ 0
   :whitepieces 0
   :blackpieces 0
   :allpieces   0
   :enpassent   0 })

(defmacro thread-it [& [first-expr & rest-expr]]
   (if (empty? rest-expr)
     first-expr
    `(let [~'it ~first-expr]
       (thread-it ~@rest-expr))))

(defn set-piece [game-state piece dest]
  (thread-it game-state
    (update-in it [piece] bit-or (bit-set 0 dest))
      (assoc-in it [:board dest] piece)
        (assoc-in it [:whitepieces] (reduce #(bit-or %1 (%2 it)) 0 [:R :N :B :Q :K :P]))
          (assoc-in it [:blackpieces] (reduce #(bit-or %1 (%2 it)) 0 [:r :n :b :q :k :p]))
            (assoc-in it [:allpieces] (bit-or (:whitepieces it) (:blackpieces it)))))

(defn move-piece [game-state piece from dest]
  (let [captured (get-in game-state [:board dest])]
    (-> game-state
        (assoc-in [:board from] :_)
           (update-in [piece] bit-xor (bit-set 0 from))
              (assoc-in [captured] (bit-xor (game-state captured) (bit-set 0 dest)))
              (set-piece piece dest))))


(defn create-board-fn [coll]
  (reduce #(set-piece %1 (first %2) (second %2)) empty-board coll))

(def initial-board
  (create-board-fn
          '([:r 63] [:n 62] [:b 61] [:q 59] [:k 60] [:b 58] [:n 57] [:r 56]
            [:p 55] [:p 54] [:p 53] [:p 52] [:p 51] [:p 50] [:p 49] [:p 48]
            [:P 15] [:P 14] [:P 13] [:P 12] [:P 11] [:P 10] [:P  9] [:P  8]
            [:R  7] [:N  6] [:B  5] [:K  4] [:Q  3] [:B  2] [:N  1] [:R  0])))

(defn read-fen [fen-str]
  (let [
        other-board-impl (other-impl-read-fen fen-str)
        squares (flatten  (reverse (:board other-board-impl)))
        squares (map-indexed vector squares)
        squares (map reverse squares)
        ] (create-board-fn squares)))


(defn pieces-by-turn [game-state]
  (if (= (game-state :turn) :w)
      (:whitepieces game-state)
      (:blackpieces game-state)))

(defn print-board [game-state]
  (print-board-vector (:board game-state)))
