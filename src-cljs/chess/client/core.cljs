(ns chess.client.core
  (:require [clojure.browser.repl :as repl]
            [one.dispatch :as disp]))

;;(repl/connect "http://localhost:9000/repl")

(defn log [& stuff]
  (doseq [s stuff]
    (.log js/console s)))

(def BOARD-SIZE 400)

(def STATE (atom {}))

(defn current-state []
  @STATE)

(defn update-state [update-fn]
  (swap! STATE update-fn))

(defn rgb [r g b]
  [r g b])

(def colors {:light-field (rgb 255 255 255)
             :dark-field (rgb 173 173 173)
             :field-focus (rgb 80 80 80)})

(defn pos [x-from-upper-left y-from-upper-left]
  [x-from-upper-left y-from-upper-left])

(defn map-alternating [coll & fs]
  (map #(%1 %2)
       (cycle fs)
       coll))

(defn light-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:light-field colors))
   #(assoc % :rgb (:dark-field colors))))

(defn dark-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:dark-field colors))
   #(assoc % :rgb (:light-field colors))))

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
       (map-alternating (partition 8 fields) light-row dark-row)))))

(defn rgb-string [[r g b]]
  (str "rgba(" r "," g "," b ",1.0)")) ;; alpha value probably not required

(defn set-fill-style [ctx rgb]
  (set! (.-fillStyle ctx) (rgb-string rgb))
  ctx)

(defn set-stroke-style [ctx rgb]
  (set! (.-strokeStyle ctx) (rgb-string rgb))
  ctx)

(defn set-line-width [ctx w]
  (set! (.-lineWidth ctx) w)
  ctx)

(defn default-line-width [ctx]
  (set-line-width ctx 1.0))

(defn draw-filled-rectangle [ctx [x y] width height]
  (.fillRect ctx x y width height)
  ctx)

(defn draw-rectangle-outline [ctx [x y] width height]
  (.strokeRect ctx x y width height)
  ctx)

(defn clear-rectangle [ctx [x y] width height]
  (.clearRect ctx x y width height)
  ctx)

(defn draw-field [context {:keys [pos width height rgb]}]
  (-> context
      (set-fill-style rgb)
      (draw-filled-rectangle pos width height)))

(defn draw-board-on [context]
  (let [board-rows (chessboard-rows BOARD-SIZE)]
    (doseq [field (apply concat board-rows)]
      (draw-field context field))
    context))

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

(defn add-field-marker [context {:keys [pos width height]} rgb]
  (set-stroke-style context rgb)
  (doseq [i (range 1 5)]
    (draw-rectangle-outline context
                            (map #(+ % i) pos)
                            (- width (* 2 i))
                            (- height (* 2 i)))))

(defn remove-field-marker [context {:keys [pos width height rgb] :as field}]
  ;; TODO
  (add-field-marker context field rgb)
  (add-field-marker context field rgb)
  (add-field-marker context field rgb)
  (add-field-marker context field rgb))

(def field-focus-lost-reaction
  (disp/react-to #{:field-focus-lost}
                 (fn [event-id context field]
                   (remove-field-marker context field))))

(def field-focus-reaction
  (disp/react-to #{:field-focus}
                 (fn [event-id context field]
                   (add-field-marker context field (:field-focus colors)))))

(defn canvas-mousemove-handler [context event]
  (update-state (fn [state]
                  (let [previous-field (:focused-field state)
                        new-field (field-at (mouse-pos-relative-to-canvas event))]
                    (when (not= previous-field new-field)
                      (when previous-field
                        (disp/fire :field-focus-lost context previous-field))
                      (when new-field
                        (disp/fire :field-focus context new-field)))
                    (assoc state :focused-field new-field)))))

;; TODO reuse Clojure code (lein cljsbuild crossover feature?)
(def game-state {:board (vec (concat
                              (vector [:r :n :b :q :k :b :n :r] (vec (repeat 8 :p)))
                              (repeat 4 (vec (repeat 8 :_)))
                              (vector (vec (repeat 8 :P)) [:R :N :B :Q :K :B :N :R]))),
                 :turn :w
                 :rochade #{:K :Q :k :q}})

(def white-pieces #{:R :N :B :Q :K :P})

(def black-pieces #{:r :n :b :q :k :p})

(defn white? [x]
  (contains? white-pieces x))

(defn black? [x]
  (contains? black-pieces x))

(defn piece? [x]
  (or (white? x) (black? x)))

(defn draw-piece [context piece-img {:keys [pos width height]}]
  (.drawImage context piece-img (first pos) (second pos))
  context)

(defn img-src [piece]
  (let [color (cond (white? piece) "white"
                    (black? piece) "black"
                    :else nil)]
    (when color
      (str "/images/" color "_" (.toLowerCase (name piece)) ".png"))))

(defn load-piece-img [piece on-loaded-fn]
  (let [image (js/Image.)]
    (set! (.-onload image) #(on-loaded-fn image))
    (set! (.-src image) (img-src piece))))

(defn render-pieces [context piece-rows]
  (let [pieces (vec (apply concat piece-rows))
        fields (vec (apply concat (chessboard-rows BOARD-SIZE)))
        indexes (range (count pieces))]
    (doseq [i indexes] ;; (map (fn [p f] ...) pieces fields) didn't work, don't know why
      (let [p (nth pieces i)
            f (nth fields i)]
        (when (piece? p)
          (load-piece-img p (fn [img]
                              (draw-piece context img f))))))))

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      context (.getContext canvas "2d")]
  (-> js/document
      (.getElementById "chess-board")
      (.appendChild canvas))
  (draw-board-on context)
  (render-pieces context (:board game-state))
  (.addEventListener canvas "mousemove" (partial canvas-mousemove-handler context) false))