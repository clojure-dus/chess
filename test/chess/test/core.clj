(ns chess.test.core
  (:use [chess.core])
  (:use [clojure.test]))

(deftest test-figure-at
  (testing "figure-at should return the correct figure for the given board and position"
    (let [fut (partial figure-at initial-board)]
    (are [x y] (= x y)
     :r (fut 0 0)
     :n (fut 0 1)
     :b (fut 0 2)
     :p (fut 1 0)
     :_ (fut 5 2)
     :P (fut 6 0)))))

(deftest test-move-figure
  (let [new-board (move-figure initial-board 1 0 2 0)]
    (are [x y] (= x y)
         :_ (figure-at new-board 1 0 )
         :p (figure-at new-board 2 0 ))))
   