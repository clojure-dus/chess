(ns chess.move-selection
  (:use [chess.move-generator :only (generate-moves)])
  (:use [chess.board-rating :only (rate)])
  (:use [chess.core :only (move-piece)]))

(defn moves2boards [moves game-state]
  (map (fn [[pos1 pos2]] (move-piece game-state pos1 pos2)) moves))


(defn select-move  [game-state]
  (let [possible-moves  (generate-moves game-state)
        possible-states (moves2boards possible-moves game-state)
        states2moves    (zipmap possible-states possible-moves)
        ratedstates     (map rate possible-states)
        moves2rates     (zipmap possible-moves ratedstates)
        max-rate        (apply max ratedstates)]
    (first (filter #(= max-rate (val %)) moves2rates))))

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


(defn rate-recursive [game-state depth max-depth]
  (let [result-boards (moves2boards (generate-moves game-state) game-state)]
    (if (= depth max-depth)
      (rate game-state)
      (let [rates (pmap (fn [board] (rate-recursive (change-turn board) (inc depth) max-depth)) result-boards)]
          (apply max rates)))))

(defn min-max [game-state depth]
   (let [possible-moves  (generate-moves game-state)
        possible-states (map (fn [[pos1 pos2]] (move-piece game-state pos1 pos2)) possible-moves)
        states2moves    (zipmap possible-states possible-moves)
        ratedstates     (pmap #(rate-recursive % 1 depth) possible-states)
        moves2rates     (zipmap possible-moves ratedstates)]
        moves2rates))
