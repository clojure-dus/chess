(ns chess.ai.move-selection.min-max
  (:require [clojure.core.reducers :as r])
  (:use [chess.movelogic.move-generator])
  (:use [chess.ai.rating.board-rating :only (rate)])
  (:use [clojure.java.io])
  (:use [clojure.pprint]))




(def MAXRATING 9999999)


(defn moves2boards [moves game-state]
  "creates new game-states for the given boards"
  (into [] (r/map #(move2board % game-state) moves)))

(defn pprint-move [[from to]]
  (let [chars (seq "abcdefgh")
        f (fn [[x y]] (str (nth chars x) (inc y))) ]
    (str (f from) "->" (f to))))

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

(defn checkmated-rating [ depth ]
   (if (= 0 (mod depth 2))
     (* -1 MAXRATING)
     MAXRATING))

(defn rate-board [game-state depth]
  (if (checkmated? game-state)
    (checkmated-rating depth)
    (rate game-state)))

(defn min-or-max
  ([c depth is-checkmated]
     (if is-checkmated
       (checkmated-rating depth)
       (min-or-max c depth)))
  ([c depth]
     (if (= 0 (mod depth 2))
       (apply max c)
       (apply min c))))

(defn filter-non-check-moves [game-state possible-moves is-check]
  (if is-check
    (into [] (r/filter #(not (test-check? (move2board % game-state))) possible-moves))
    possible-moves))

(defn build-tree
  ([game-state max-depth] (build-tree game-state 0 max-depth [] nil))
  ([game-state depth max-depth r step]
     (if (= depth max-depth)
       {:score (rate-board game-state depth)}
       (let [is-check (test-check?  game-state)
             possible-moves (filter-non-check-moves game-state (generate-moves  game-state) is-check)
             is-checkmated (and is-check (empty? possible-moves))
             subtree  (if is-checkmated nil (pmap #(build-tree (change-turn (move2board % game-state)) (inc depth) max-depth [] %) possible-moves))
             rates    (into [] (r/flatten (r/map :score subtree)))
             max-rate (min-or-max rates depth is-checkmated)
             max-step (get (zipmap rates possible-moves) max-rate)]
             {:score max-rate :max-step max-step}))))


(defn min-max [game-state max-depth]
  (:max-step (build-tree game-state max-depth)))

(defn select-move
  "Generates the next move for the given game-state.
   Returns seq with two positions 'from' and 'two'."
  [game-state]
  (min-max game-state 2))
