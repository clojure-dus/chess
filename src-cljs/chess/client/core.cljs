(ns chess.client.core
  (:require [clojure.browser.repl :as repl]
            [one.dispatch :as disp]))

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

(defn map-alternating [coll & fs]
  (map #(%1 %2)
       (cycle fs)
       coll))

(defn white-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:white colors))
   #(assoc % :rgb (:black colors))))

(defn black-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:black colors))
   #(assoc % :rgb (:white colors))))

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
       (map-alternating (partition 8 fields) white-row black-row)))))

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
        (draw-rectangle-outline (pos 0 0) BOARD-SIZE BOARD-SIZE))))

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

(defn remove-field-marker [context field]
  (log (str "removing marker on " field)))

(defn add-field-marker [context field]
  (log (str "adding marker on " field)))

(def left-field-reaction
  (disp/react-to #{:left-field}
                 (fn [event-id context field]
                   (remove-field-marker context field))))

(def entered-field-reaction
  (disp/react-to #{:entered-field}
                 (fn [event-id context field]
                   (add-field-marker context field))))

(defn canvas-mousemove-handler [context event]
  (update-state (fn [state]
                  (let [previous-field (:mouse-field state)
                        new-field (field-at (mouse-pos-relative-to-canvas event))]
                    (when (not= previous-field new-field)
                      (when previous-field
                        (disp/fire :left-field context previous-field))
                      (when new-field
                        (disp/fire :entered-field context new-field)))
                    (assoc state :mouse-field new-field)))))

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      context (.getContext canvas "2d")]
  (-> js/document
      (.getElementById "chess-board")
      (.appendChild canvas))
  (draw-board-on context)
  (.addEventListener canvas "mousemove" (partial canvas-mousemove-handler context) false))