(ns chess.move-selection
  (:require [chess.moves-api :only (generate-moves) :as moves])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (move-piece pos-of-piece piece-at)])
  (:use [clojure.java.io])
  (:use [clojure.pprint]))

(def CHECKMATED 9999999)

(defn move2board [[pos1 pos2] game-state]
  (move-piece game-state pos1 pos2))

(defn moves2boards [moves game-state]
  "creates new game-states for the given boards"
  (map #(move2board % game-state) moves))

(defn pprint-move [[from to]]
  (let [chars (seq "abcdefgh")
        f (fn [[x y]] (str (nth chars x) (inc y))) ]
    (str (f from) "->" (f to))))
     
(defn whites-turn? [game-state]
  (= :w (:turn game-state)))

(defn change-turn [game-state]
  "changes the turn to the next player"
  (if (whites-turn? game-state)
    (assoc game-state :turn :b)
    (assoc game-state :turn :w)))

(defn check? [game-state]
  "checks if the given board is in check"
  (let [opponent-moves (moves/generate-moves (change-turn game-state))
        king           (if (whites-turn? game-state) :K :k)
        kings-pos      (pos-of-piece game-state king)]
     (true? (some (fn [[_ to]] (= king (piece-at game-state to))) opponent-moves))))


(defn checkmated?
  ([game-state]
   (let [new-boards (moves2boards (moves/generate-moves game-state) game-state)]
      (checkmated? game-state new-boards)))
  ([game-state new-boards]
   (if (not (check? game-state))
    false 
    (every? check? new-boards))))

(defn select-max-rate  [game-state]
  "returns the best rate for all possible moves on the given board"
  (let [possible-moves  (moves/generate-moves game-state)
        possible-states (moves2boards possible-moves game-state)
        ratedstates     (map rate possible-states)
        max-rate        (if (checkmated? game-state possible-states) CHECKMATED (apply max ratedstates))]
        max-rate))

(defn rate-recursive [game-state depth max-depth]
  (let [result-boards (moves2boards (moves/generate-moves game-state) game-state)]
    (if (= depth max-depth)
      (let [max-rate (select-max-rate (change-turn game-state))]
          max-rate)
      (let [rates (pmap (fn [board] (rate-recursive (change-turn board) (inc depth) max-depth)) result-boards)]
         (if (= 0 (mod depth 2))
           (apply max rates)
           (apply min rates))))))


(defn min-or-max [c depth]
  (if (= 0 (mod depth 2))
    (apply max c)
    (apply min c)))


(defn build-tree
  ([game-state max-depth] (build-tree game-state 0 max-depth [] nil))
  ([game-state depth max-depth r step]
     (if (= depth max-depth) {:score (select-max-rate game-state) :game-state game-state :former-step step}
              (let [possible-moves (moves/generate-moves game-state)
                    possible-states (moves2boards possible-moves game-state)
                    subtree  (pmap #(build-tree (change-turn (move2board % game-state)) (inc depth) max-depth [] %) possible-moves)
                    rates    (flatten (map :score subtree))
                    max-rate (apply max rates)
                    moves2rates  (zipmap possible-moves rates)
                    max-step (ffirst (filter #(= max-rate (val %)) moves2rates))]
                { :score max-rate :max-step max-step :game-state game-state  :former-step step :tree subtree}))))

(defn trace-tree [game-state max-depth]
  (with-open [w (writer "tree.trace")]
    (pprint (build-tree game-state max-depth) w)))

(defn min-max [game-state max-depth]
  (:max-step (build-tree game-state max-depth)))

(defn select-move [game-state]
  (min-max game-state 2))


