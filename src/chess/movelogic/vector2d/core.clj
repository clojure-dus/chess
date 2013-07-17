(ns chess.movelogic.vector2d.core
  (:use [clojure.pprint])
  (:use [chess.movelogic.protocol  :only [read-fen->map]])
  (:require [clojure.core.reducers :as r]))

                                        ; UPPERCASE -> white
                                        ; lowercase -> black


(defrecord GameState [board turn rochade])

(def initial-board
  (map->GameState {:board (vec (concat
                                (vector [:r :n :b :q :k :b :n :r] (vec (repeat 8 :p)))
                                (repeat 4 (vec (repeat 8 :_)))
                                (vector (vec (repeat 8 :P)) [:R :N :B :Q :K :B :N :R]))),
                   :turn :w
                   :rochade #{:K :Q :k :q}}))

(defn read-fen [s]
  "reads a string in Forsyth-Edwards notation and returns internal chess board representation"
  (map->GameState (read-fen->map s)))


(defn print-board[game-state]
  (println "----- vector2d version -----")
  (clojure.pprint/pprint  game-state))

(def white-pieces #{:R :N :B :Q :K :P})
(def black-pieces #{:r :n :b :q :k :p})

(defn white? [x] (contains? white-pieces x))

(defn black? [x] (contains? black-pieces x))

(defn piece? [x] (or (white? x) (black? x)))

(defn piece-at
  "returns the piece keyword for the given game-state and coordinates - (0, 0) is lower left corner"
  [game-state [x y]]
  (get-in game-state [:board (- 7 y) x ]))

(def all-positions
  "a collection of positions/coordinates on a chess board"
  (for [x (range 8)
        y (range 8)]
    [x y]))

(defn filter-my-positions [color-fn game-state]
  "returns all positions which are occupied by the given color"
  (r/filter (fn [pos] (color-fn (piece-at game-state pos))) all-positions))

(defn pos-on-board? "checks if a positition is on the chess board"
  [[x y]]
  (every? (fn [n] (<= 0 n 7)) [x y]))

(defn pos-empty?
  "is a position on the given game-state empty"
  [game-state position]
  (= :_ (piece-at game-state position)))

(defn set-piece [game-state [x y] piece]
  "sets a piece on the given coordinate without any rule checking"
  (assoc-in game-state [:board (- 7 y) x] piece))

(defn move-piece
  "moves a piece on the given game-state from x1,y2 to x2,y2 without any rule checking"
  [game-state pos1 pos2]
  {:pre [(pos-on-board? pos1)
         (pos-on-board? pos2)]}
  (let [fig (piece-at game-state pos1)]
    (set-piece (set-piece game-state pos1 :_) pos2 fig)))


(defn pos-of-piece [game-state p]
  "returns the positions [x y] of the given piece on the board"
  (into [] (r/filter (fn [pos] (= p (piece-at game-state pos))) all-positions)))

