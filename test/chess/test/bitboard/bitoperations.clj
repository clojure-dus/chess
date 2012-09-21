(ns chess.test.core
  (:use [chess.bitboard.bitoperations])
  (:use [clojure.test]))

(deftest test-find-first-one-bit
  (testing "check if the first right index is found"
    (is (= 0 (find-first-one-bit
              2r0000000000000000000000000000000000000000000000000000000000000001))))
    (is (= 63 (find-first-one-bit
               (unchecked-long 2r1000000000000000000000000000000000000000000000000000000000000000))))
    (is (= 7 (find-first-one-bit
              2r0000000000000000000000000000000000000000000000000000011110000000)))
    (is (= 0 (find-first-one-bit
               (unchecked-long 2r1111111111111111111111111111111111111111111111111111111111111111))))
    (is (= 4 (find-first-one-bit
               (unchecked-long 2r1111111111111111111111111111111111111111111111111111111111110000)))))
(run-tests)
