(ns chess.test.movelogic.vector2d.moves-impl
  (:use [chess.movelogic.vector2d.core :only (initial-board move-piece)])
  (:use [chess.movelogic.vector2d.moves-impl])
  (:use [clojure.test])
  (:use [chess.movelogic.vector2d.directions]))

(deftest test-steps-right
  (are [x y] (= x y)
       (list (list 2 3) (list 3 3) (list 4 3) (list 5 3) (list 6 3) (list 7 3)) (steps-right [1 3])))

(deftest test-steps-left
  (are [x y] (= x y)
       (list (list 1 3) (list 0 3)) (steps-left [2 3])))

(deftest test-steps-up
  (are [x y] (= x y)
       (list (list 2 4) (list 2 5) (list 2 6) (list 2 7)) (steps-up [2 3])))

(deftest test-steps-down
  (are [x y] (= x y)
       (list (list 2 2) (list 2 1) (list 2 0)) (steps-down [2 3])))


(deftest test-fetch-positions
  (is (= '((0 0) (0 1) (1 0) (1 1) (2 0) (2 1) (3 0) (3 1) (4 0) (4 1) (5 0) (5 1) (6 0) (6 1) (7 0) (7 1)) (fetch-positions (enrich initial-board))))
  (is (= '((0 6) (0 7) (1 6) (1 7) (2 6) (2 7) (3 6) (3 7) (4 6) (4 7) (5 6) (5 7) (6 6) (6 7) (7 6) (7 7)) (fetch-positions (assoc (enrich initial-board) :turn :b)))))


(deftest test-make-move
  (testing "invalid moves return nil"
    (is (= nil (make-move initial-board [0 1] [0 4]))))
  (testing "valid moves return a gamestate with an updated board"
    (is (= [[:r :n :b :q :k :b :n :r]
            [:p :p :p :p :p :p :p :p]
            [:_ :_ :_ :_ :_ :_ :_ :_]
            [:_ :_ :_ :_ :_ :_ :_ :_]
            [:P :_ :_ :_ :_ :_ :_ :_]
            [:_ :_ :_ :_ :_ :_ :_ :_]
            [:_ :P :P :P :P :P :P :P]
            [:R :N :B :Q :K :B :N :R]]
           (:board (move-piece initial-board [0 1] [0 3]))))))

