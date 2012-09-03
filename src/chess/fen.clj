(ns chess.fen
  (:use [clojure.string :only (split)]))

(defn- int-str [s]
  (try (Integer/parseInt (str s)) (catch Exception e)))

(defn- read-fen-line [s]
  (vec (flatten (map #(let [i (int-str %) k (if i (take i (repeat :_)) (keyword (str %)))] k) s))))

(defn read-fen [s]
  "reads a string in Forsyth-Edwards notation and returns internal chess board representation"
  (let [r (split s #"[/ ]")]
    {:board (vec (map read-fen-line (take 8 r)))
     :turn (keyword (nth r 8))
     :rochade (set (map #(keyword (str %)) (seq (nth r 9))))}))