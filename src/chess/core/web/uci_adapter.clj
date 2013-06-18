(ns chess.xboard-adapter)

;; pure functions

(defn position [_ fen-string]
  (parse fen-string))

(defn go [game-state]
  ...)

;; stateful api

(defonce game-state (atom nil))

(defn position!
  "set up the position described in fenstring on the internal board"
  [fen-string]
  (swap! game-state position fen-string))

(defn go!
  "start calculating on the current position set up with the 'position' command"
  []
  (swap! game-state go))