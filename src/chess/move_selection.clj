(ns chess.move-selection
  (:use [chess.move-generator :only (generate-moves)])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (move-piece)]))

(defn moves2boards [moves game-state]
  "creates new game-states for the given boards"
  (map (fn [[pos1 pos2]] (move-piece game-state pos1 pos2)) moves))

(defn pprint-move [[from to]]
  (let [chars (seq "abcdefgh")
        f (fn [[x y]] (str (nth chars x) (inc y))) ]
    (str (f from) "->" (f to))))

(defn whites-turn? [game-state]
  (= :w (:turn game-state)))

(defn change-turn [game-state]
  (if (whites-turn? game-state)
    (assoc game-state :turn :b)
    (assoc game-state :turn :w)))

(defn select-max-rate  [game-state]
  "returns the best rate for all possible moves on the given board"
  (let [possible-moves  (generate-moves game-state)
        possible-states (moves2boards possible-moves game-state)
        ratedstates     (map rate possible-states)
        max-rate        (apply max ratedstates)]
    max-rate))

(defn rate-recursive [game-state depth max-depth]
  (let [result-boards (moves2boards (generate-moves game-state) game-state)]
    (if (= depth max-depth)
      (select-max-rate (change-turn game-state))
      (let [rates (pmap (fn [board] (rate-recursive (change-turn board) (inc depth) max-depth)) result-boards)]
         (if (= 0 (mod depth 2))
           (apply max rates)
           (apply min rates))))))

(defn min-max [game-state max-depth]
  (let [possible-moves  (generate-moves game-state)
        possible-states (moves2boards possible-moves game-state)
        rated-states (map #(rate-recursive % 1 max-depth) possible-states)
        max-rate     (apply max rated-states)
        moves2rates  (zipmap possible-moves rated-states)]
        (ffirst (filter #(= max-rate (val %)) moves2rates))))

(defn select-move [game-state]
  (min-max game-state 2))