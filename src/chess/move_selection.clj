(ns chess.move-selection
  (:require [chess.moves-api :only (generate-moves) :as moves])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (move-piece pos-of-piece piece-at)])
  (:use [clojure.java.io])
  (:use [clojure.pprint]))

(def MAXRATING 9999999)

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

(defn rate-board [game-state]
  (if (checkmated? game-state)
    MAXRATING
    (rate game-state)))

(defn min-or-max
  ([c depth is-checkmated]
     (if (not is-checkmated)
       (min-or-max c depth)
       (if (= 0 (mod depth 2))
         (* -1 MAXRATING)
         MAXRATING)))
  ([c depth]
     (if (= 0 (mod depth 2))
       (apply max c)
       (apply min c))))

(defn build-tree
  ([game-state max-depth] (build-tree game-state 0 max-depth [] nil))
  ([game-state depth max-depth r step]
     (if (= depth max-depth)
       {:score (rate-board game-state) :game-state game-state :former-step step}
       (let [possible-moves (moves/generate-moves game-state)
             possible-states (moves2boards possible-moves game-state)
             is-checkmated (checkmated? game-state possible-states)
             subtree  (if (not is-checkmated) (pmap #(build-tree (change-turn (move2board % game-state)) (inc depth) max-depth [] %) possible-moves) nil)
             rates    (flatten (map :score subtree))
             max-rate (min-or-max rates depth is-checkmated)
             rates2moves  (zipmap rates possible-moves)
             max-step (get rates2moves (first (filter #(= max-rate %) rates)))]
             { :score max-rate :max-step max-step :game-state game-state  :former-step step :tree subtree}))))

(defn trace-tree [game-state max-depth]
  (with-open [w (writer "tree.trace")]
    (pprint (build-tree game-state max-depth) w)))

(defn min-max [game-state max-depth]
  (:max-step (build-tree game-state max-depth)))

(defn select-move [game-state]
  (min-max game-state 2))


