(ns chess.movelogic.protocol
(:use [clojure.string :only (split)])
(:require [clojure.core.reducers :as r]))

(defprotocol MoveGenerator
  (generate-moves [this])
  (possible-moves [this coord])
  (move-piece [this from to]) 
  (test-check? [this])
  (filter-positions-by-color [this white])   
  (get-piece [this position])
  (print-board [this]))

(def piece-map
  {:r :rook, :R :rook,
   :p :pawn, :P :pawn,
   :n :knight,:N :knight,
   :b :bishop,:B :bishop,
   :q :queen, :Q :queen,
   :k :king,  :K :king})

(defn piece [game-state position]
    "returns a keyword (color-neutral) for the piece on the given position"
    (let [p (get-piece game-state position)]
      (p piece-map)))

(defn whites-turn? [game-state]
  (= :w (:turn game-state)))

(defn change-turn
  "changes the turn to the next player"
  [game-state]
  (if (whites-turn? game-state)
    (assoc game-state :turn :b)
    (assoc game-state :turn :w)))


(defn move2board [[pos1 pos2] game-state]
   (move-piece  game-state pos1 pos2))

(defn- moves2boards [moves game-state]
    "creates new game-states for the given boards"
    (into [] (r/map #(move2board % game-state) moves)))

(defn checkmated?
    ([game-state]
       (let [new-boards (moves2boards (generate-moves game-state) game-state)]
         (checkmated? game-state new-boards)))
    ([game-state new-boards]
       (checkmated? game-state new-boards (test-check?  game-state)))
    ([game-state new-boards is-check]
       (if (not is-check)
         false
         (every? test-check? new-boards))))

  (defn filter-non-check-moves [game-state possible-moves is-check]
    (if is-check
      (into [] (r/filter #(not (test-check? (move2board % game-state))) possible-moves))
      possible-moves))


(defn- int-str [s]
  (try (Integer/parseInt (str s)) (catch Exception e)))

(defn- read-fen-line [s]
  (vec (flatten (map #(let [i (int-str %) k (if i (take i (repeat :_)) (keyword (str %)))] k) s))))

(defn read-fen->map [s]
  (let [r (split s #"[/ ]")]
                     {:board (vec (map read-fen-line (take 8 r)))
                      :turn (keyword (nth r 8))
                      :rochade (set (map #(keyword (str %)) (seq (nth r 9))))}))
