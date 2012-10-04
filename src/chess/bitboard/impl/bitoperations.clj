(ns chess.bitboard.impl.bitoperations)
(import 'chess.bitboard.impl.BitOps)

(defn unsigned-shift-right[x n]
  (BitOps/unsignedShiftRight x n))

(let [bit-vec  (loop [bitmasks (vector-of :long 2r1) n 0]
    (if (= 63 n) bitmasks
        (recur  (conj bitmasks (bit-shift-left (last bitmasks) 1)) (inc n))))]
  (defn square->bit ^long [x] (bit-vec x)))


(let [ index-64 (into-array Integer/TYPE [
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

(defmacro for-bitmap [[key bitmap] & body]
" iterates for each bit set in bitmap. the index is bind to key. returns a vector of body results"
  `(loop [result# []
          pieces#  ~bitmap]
     (if (= 0 pieces#)
       result#
       (let [~key (find-first-one-bit pieces#)]
         (recur (conj result# (do ~@body))
                (bit-xor pieces# (bit-set 0 ~key)))))))
(defn vector->bit [vect]
  (if (empty? vect)
    0
    (reduce (fn [x y] 0 (bit-or y (bit-shift-left x 1))) (reverse vect))))

(defn bit->vector[bits count]
  (loop [result [] n 0]
    (let [bit (if (bit-test bits n) 1 0)]
      (if (= n count)
        result
        (recur (conj result bit) (inc n))))))

(defn indexed-bits [func bit-vector]
 (filter (fn[[idx bit]] (= 1 bit)) (map-indexed func bit-vector)))

(defn flipVertical[b]
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
  (flipVertical (flipDiagonalA1H8 b)))

(defn only-inner-board [sq]
  (and  (> sq -1) (< sq 64)))

(defn rotate-45-bitboard [bitboard  rotate-fn]
  (let [bit-vect    (bit->vector bitboard 64)
        bit-vect    (indexed-bits vector bit-vect)
        update-vect (map rotate-fn bit-vect)
        update-vect (filter only-inner-board update-vect)
        result       (apply vector (repeat 64 0))]
    (vector->bit (reduce (fn [r sq] (assoc r sq 1)) result update-vect))))

(defn square-45-clockwise [rotate-pos]
  (fn [[square _]]
     (let [delta (- rotate-pos square)]
        (+ (* 7 delta) rotate-pos))))

(defn square-45-anticlockwise [rotate-pos]
(fn [[square _]]
     (let [delta (- square rotate-pos)]
        (+ (* 9 delta) rotate-pos))))

(defn rotate-bitboard-45-clockwise [rotate-pos bitboard]
  (rotate-45-bitboard bitboard (square-45-clockwise rotate-pos)))

(defn rotate-bitboard-45-anticlockwise [rotate-pos bitboard]
  (rotate-45-bitboard bitboard (square-45-anticlockwise rotate-pos)))
