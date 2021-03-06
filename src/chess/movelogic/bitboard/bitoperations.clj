(ns chess.movelogic.bitboard.bitoperations
  (:use [clojure.core.protocols]))
(import 'chess.movelogic.bitboard.BitOps)
(require '[clojure.core.reducers :as r])

(comment (set! *warn-on-reflection* true))
(defn unsigned-shift-right[x n]
  "needed because clojure 1.4 doesn't support unsigned right shift"
  (BitOps/unsignedShiftRight x n))

(let [bit-vec  (loop [bitmasks (vector-of :long 2r1) n 0]
    (if (= 63 n) bitmasks
        (recur  (conj bitmasks (bit-shift-left (last bitmasks) 1)) (inc n))))]
  (defn square->bit ^long [x] (bit-vec x)))

(let [index-64 (into-array Integer/TYPE [
            63,  0, 58,  1, 59, 47, 53,  2,
            60, 39, 48, 27, 54, 33, 42,  3,
            61, 51, 37, 40, 49, 18, 28, 20,
            55, 30, 34, 11, 43, 14, 22,  4,
            62, 57, 46, 52, 38, 26, 32, 41,
            50, 36, 17, 19, 29, 10, 13, 21,
            56, 45, 25, 31, 35, 16,  9, 12,
            44, 24, 15,  8, 23,  7,  6,  5
                                       ])]
  (defn ^int find-first-one-bit[^long bb]
    " return index of least significant one-bit. bb assumed to be 64 bit"
       (let [debruin 0x07EDD5E59A4E28C2
             term  (bit-and bb (unchecked-negate bb))
             index (unsigned-shift-right (unchecked-multiply term debruin) 58)]
       (aget ^ints index-64 index))))


 (extend-protocol CollReduce
    java.lang.Long
    (coll-reduce
      ([coll f] (internal-reduce 0 coll f))
      ([coll f val] (internal-reduce coll f val))))

  (extend-protocol InternalReduce
    java.lang.Long
    (internal-reduce [bitmap f val]
      (loop [val val  pieces bitmap]
       (if (= 0 pieces)
         val
         (let [index (find-first-one-bit pieces)]
          (recur (f val index) (bit-xor pieces (bit-set 0 index))))))))

(defmacro bit-and? [& rest]
  "return true on bit-anded bitmaps"
  `(not= 0 (bit-and ~@rest )))

(defmacro some-bitmap [pred bitmap]
  "iterates over all bitmaps 1s (squares)
 and applies pred  (one arg fn which takes a square)  to each set square. stops at the first
 pred == true. similar to core/some"
  `(loop [pieces#  ~bitmap]
     (when (not= 0 pieces#)
       (let [square# (find-first-one-bit pieces#)]
         (or (~pred square#)  (recur (bit-xor pieces# (bit-set 0 square#))))))))

(defn vector->bit [vect]
"converts a vector consisting of only zeros and ones to a 64 bitboard."
  (if (empty? vect)
    0
    (reduce (fn [x y] 0 (bit-or y (bit-shift-left x 1))) (reverse vect))))

(defn bit->vector[bits count]
  "converts a bitboard to a vector of ones and zeros."
  (loop [result [] n 0]
    (let [bit (if (bit-test bits n) 1 0)]
      (if (= n count)
        result
        (recur (conj result bit) (inc n))))))

(defn flipVertical[b]
  "flips all bits of a 8 bit mask to their vertical position"
  (let [h1  0x00FF00FF00FF00FF
        h2  0x0000FFFF0000FFFF
        b   (bit-or(bit-and(bit-shift-right b 8) h1)
                                   (bit-shift-left (bit-and b h1) 8))
        b   (bit-or(bit-and (bit-shift-right b 16) h2)
                                   (bit-shift-left (bit-and b h2) 16))
        b   (bit-or(unsigned-shift-right b 32) (bit-shift-left b 32))] b))

(defn flipDiagonalA1H8[^long b]
  (let [h1 (unchecked-long 0x5500550055005500)
        h2 (unchecked-long 0x3333000033330000)
        h3 (unchecked-long 0x0F0F0F0F00000000)
        t  (bit-and h3 (bit-xor b (bit-shift-left b 28)))
        b  (bit-xor b  (bit-xor t (bit-shift-right t 28)))
        t  (bit-and h2 (bit-xor b (bit-shift-left b 14)))
        b  (bit-xor b  (bit-xor t (bit-shift-right t 14)))
        t  (bit-and h1 (bit-xor b (bit-shift-left b 7)))
        b  (bit-xor b  (bit-xor t (bit-shift-right t 7)))] b))

(defn rotate90-bitboard-clockwise [b]
  "rotates a horizontal 8bit mask to its vertical position"
  (flipVertical (flipDiagonalA1H8 b)))

(defn only-inner-board [sq]
  (and  (> sq -1) (< sq 64)))

(defn to-long-array-2d
  "creates a 2 dim array where each array contains 64 longs"
  [^java.util.Collection coll]
    (let [ret (make-array Long/TYPE (. coll (size)) 64)]
      (loop [i 0 xs (seq coll)]
        (when xs
          (aset ret i (long-array (first xs)))
          (recur (inc i) (next xs))))
      ret))

(defn print-board-vector [board-vector]
 (let [abc  "    a  b  c  d  e  f  g  h \n"
       rows (map (fn[x] (str x " ")) (reverse board-vector))
       rows (partition 8 rows)
       rows  (map #(reverse %) rows)
       rows (map (fn[rank row] (vec (cons (str " " rank "  ") row)))  (range 8 0 -1) rows)
       rows (map (fn [x] (conj x "\n")) rows)
       rows (apply str (flatten rows))]
   (println)
   (print rows abc)))

(defn bitmap->board-vector [board-vector piece bitmap]
  "adds a bitmap to a board. board is a vector of piece keyowrds"
  (let [indexes   (into [] (r/map  identity bitmap))
        update-fn (fn [board-vector idx] (assoc board-vector idx piece))]
     (reduce update-fn board-vector indexes)))

(defn print-bitmap [bitmap piece]
  (println  "\nVal:" bitmap ",Bitmap :" (Long/toBinaryString bitmap))
  (let [board-vector  (mapv #(if (< % 10) (str 0 %)  %) (range 64))]
    (print-board-vector (bitmap->board-vector board-vector piece bitmap))))

(defmacro deep-aget
  "page 440 Clojure Programming. ensures that an aget on a multi-dim array
 is performed with the propertype hints set"
  ([array idx]`(aget ~array ~idx))
  ([array idx & idxs]
     (let [a-sym (gensym "a")]
       `(let [~a-sym (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
          (deep-aget ~(with-meta a-sym {:tag (-> array meta :tag)})~@ idxs)))))
