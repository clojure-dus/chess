(ns chess.bitboard.chessboard
  (:use [chess.bitboard.bitoperations])
  (:use [chess.bitboard.file-rank])
  (:use [chess.bitboard.piece-attacks])
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

(defn create-board-fn [coll]
  (reduce #(set-piece %1 (first %2) (second %2)) empty-board coll))

(def initial-board
  (create-board-fn
          '([:r 7] [:n 6] [:b 5] [:q 4] [:k 3] [:b 2] [:n 1] [:r 0]
            [:p 8] [:p 9] [:p 10] [:p 11] [:p 12] [:p 13] [:p 14] [:p 15]
            [:R 56] [:N 57] [:B 58] [:Q 59] [:K 60] [:B 61] [:N 62] [:R 63]
            [:P 55] [:P 54] [:P 53] [:P 52] [:P 51] [:P 50] [:P 49] [:P 48])))


(defn read-fen [fen-str]
  (let [
        other-board-impl (other-impl-read-fen fen-str)
        squares (flatten  (:board other-board-impl))
        squares (map-indexed vector squares)
        squares (map reverse squares)
        ] (create-board-fn squares)))

(defn move-piece [game-state piece from dest]
  (let [captured (get-in game-state [:board dest])]
    (-> game-state
        (assoc-in [:board from] :_)
           (update-in [piece] bit-xor (bit-set 0 from))
              (assoc-in [captured] (bit-xor (game-state captured) (bit-set 0 dest)))
               (set-piece piece dest))))

(defn pieces-by-turn [game-state]
  (if (= (game-state :turn) :w)
      (:whitepieces game-state)
      (:blackpieces game-state)))

(defn print-board [game-state]
 (let [
         abc  "    A  B  C  D  E  F  G  H \n"
         rows (:board game-state)
         rows (map (fn[x] (str  x " ")) rows)
         rows (partition 8 rows)
         rows (map (fn[x file] (vec(cons (str " " file "  ") x))) rows (range 1 9))
         rows (map (fn [x] (conj  x  "\n")) rows)
       rows (apply str (flatten rows)) ]
   (println)
   (print  rows abc )))


(print-board
 (read-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
