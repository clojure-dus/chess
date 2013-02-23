(ns chess.move-selection
  (:require [chess.moves-api    :only  (move-generator):as default])
  (:require [chess.bitboard.api :only (move-generator) :as bitboard])
  (:require [clojure.core.reducers :as r])
  (:use [chess.move-generator])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (change-turn)])
  (:use [clojure.java.io])
  (:use [clojure.pprint]))

(def ^:dynamic *move-engine*  (default/move-generator))

(def MAXRATING 9999999)

(defn move2board [[pos1 pos2] game-state]
  (move-piece *move-engine* game-state pos1 pos2))

(defn moves2boards [moves game-state]
  "creates new game-states for the given boards"
  (into [] (r/map #(move2board % game-state) moves)))

(defn pprint-move [[from to]]
  (let [chars (seq "abcdefgh")
        f (fn [[x y]] (str (nth chars x) (inc y))) ]
    (str (f from) "->" (f to))))

(defn checkmated?
  ([game-state]
   (let [new-boards (moves2boards (generate-moves *move-engine* game-state) game-state)]
      (checkmated? game-state new-boards)))
  ([game-state new-boards]
     (checkmated? game-state new-boards (test-check? *move-engine* game-state)))
  ([game-state new-boards is-check]
     (if (not is-check)
       false
       (every? (partial test-check? *move-engine*) new-boards))))

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
    (into [] (r/filter #(not (test-check? *move-engine* (move2board % game-state))) possible-moves))
    possible-moves))

(defn build-tree
  ([game-state max-depth] (build-tree game-state 0 max-depth [] nil))
  ([game-state depth max-depth r step]
     (if (= depth max-depth)
       {:score (rate-board game-state depth)}
       (let [is-check (test-check? *move-engine* game-state)
             possible-moves (filter-non-check-moves game-state (generate-moves *move-engine* game-state) is-check)
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
