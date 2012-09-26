(ns chess.bitboard.bitoperations)
(import 'chess.bitboard.BitOps)

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
    (reduce (fn [x y] 0 (bit-or y (bit-shift-left x 1))) vect)))

(defn bit->vector[bits count]
  (loop [result '() n 0]
    (let [bit (if (bit-test bits n) 1 0)]
      (if (= n count)
        (apply vector result)
        (recur (conj result bit) (inc n))))))
