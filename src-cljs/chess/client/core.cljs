(ns chess.client.core
  (:require [clojure.browser.repl :as repl]))

;(repl/connect "http://localhost:9000/repl")

(def BOARD-SIZE 400)

(def STATE (atom {}))

(defn current-state []
  @STATE)

(defn update-state [update-fn]
  (swap! STATE update-fn))

(defn log [& stuff]
  (doseq [s stuff]
    (.log js/console s)))

(defn rgb [r g b]
  [r g b])

(def colors {:white (rgb 255 255 255)
             :black (rgb 0 0 0)})

(defn pos [x-from-upper-left y-from-upper-left]
  [x-from-upper-left y-from-upper-left])

(defn map-alternating [f1 f2 coll]
  (map (fn [idx elem]
         (if (even? idx)
           (f1 elem)
           (f2 elem)))
       (iterate inc 0)
       coll))

(defn white-row [fields]
  (map-alternating
   #(assoc % :rgb (:white colors))
   #(assoc % :rgb (:black colors))
   fields))

(defn black-row [fields]
  (map-alternating
   #(assoc % :rgb (:black colors))
   #(assoc % :rgb (:white colors))
   fields))

(def chessboard-rows
  (memoize
   (fn [board-size]
     (let [field-size (/ board-size 8)
           steps (range 0 board-size field-size)
           positions (for [y steps x steps] (pos x y))
           fields (map (fn [pos]
                         {:pos pos
                          :width field-size
                          :height field-size})
                       positions)]
       (map-alternating white-row black-row (partition 8 fields))))))

(defn set-fill-style [ctx [r g b]]
  (set! (.-fillStyle ctx) (str "rgb(" r "," g "," b ")"))
  ctx)

(defn draw-filled-rectangle [ctx [x y] width height]
  (.fillRect ctx x y width height))

(defn draw-rectangle-outline [ctx [x y] width height]
  (.strokeRect ctx x y width height))

(defn draw-field [context {:keys [pos width height rgb]}]
  (-> context
      (set-fill-style rgb)
      (draw-filled-rectangle pos width height)))

(defn draw-board-on [context]
  (let [board-rows (chessboard-rows BOARD-SIZE)]
    (doseq [field (apply concat board-rows)]
      (draw-field context field))
    (-> context
        (set-fill-style (:black colors))
        (draw-rectangle-outline (pos 0 0) BOARD-SIZE BOARD-SIZE (:black colors)))))

(defn create-canvas [width height]
  (doto (.createElement js/document "canvas")
    (.setAttribute "width" width)
    (.setAttribute "height" height)))

;; see http://dev.opera.com/articles/view/html5-canvas-painting/
(defn mouse-pos-relative-to-canvas [event]
  (if (or (.-layerX event) (= 0 (.-layerX event)))
    (pos (.-layerX event) (.-layerY event))
    (pos (.-offsetX event) (.-offsetY event))))

(defn covers? [{:keys [pos width height]} [x y]]
  (let [[field-x field-y] pos]
    (and (<= field-x x (+ field-x width))
         (<= field-y y (+ field-y height)))))

(defn field-at [pos]
  (let [fields (apply concat (chessboard-rows BOARD-SIZE))]
    (some #(when (covers? % pos)
             %)
          fields)))

(defn canvas-mousemove-handler [event]
  (update-state (fn [state]
                  (let [previous-field (:mouse-field state)
                        new-field (field-at (mouse-pos-relative-to-canvas event))]
                    (when (not= previous-field new-field)
                      (log (str "new field " new-field)))
                    (assoc state :mouse-field new-field)))))

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      context (.getContext canvas "2d")]
  (-> js/document
      (.getElementById "chess-board")
      (.appendChild canvas))
  (draw-board-on context)
  (.addEventListener canvas "mousemove" canvas-mousemove-handler false))