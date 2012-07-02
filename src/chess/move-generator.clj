(ns chess.move-generator)

(defn generate-moves [game-state]
  (let [positions ...]
    (map (partial possible-moves game-state)
         positions)))

(defmulti possible-moves piece)

(defn piece [game-state position]
  :rook)

(defmethod possible-moves :rook
  [game-state position]
  ...)