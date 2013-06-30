(ns chess.ai.move-selection.min-max
  (:require [clojure.core.reducers :as r])
  (:use [chess.movelogic.move-generator])
  (:use [chess.ai.rating.board-rating :only (rate)])
  (:use [clojure.java.io]))

(def MAXRATING 9999999)

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
