(ns chess.client.drawing
  (:require [chess.core :as chess]))

(defn log [& stuff]
  (doseq [s stuff]
    (.log js/console s)))

(defn- rgb-string [[r g b]]
  (str "rgb(" r "," g "," b ")"))

(defn- set-fill-style [ctx rgb]
  (set! (.-fillStyle ctx) (rgb-string rgb))
  ctx)

(defn- set-stroke-style [ctx rgb]
  (set! (.-strokeStyle ctx) (rgb-string rgb))
  ctx)

(defn- set-line-width [ctx w]
  (set! (.-lineWidth ctx) w)
  ctx)

(defn- default-line-width [ctx]
  (set-line-width ctx 1.0))

(defn- draw-filled-rectangle [ctx [x y] width height]
  (.fillRect ctx x y width height)
  ctx)

(defn- draw-rectangle-outline [ctx [x y] width height]
  (.strokeRect ctx x y width height)
  ctx)

(defn- clear-rectangle [ctx [x y] width height]
  (.clearRect ctx x y width height)
  ctx)

(defn- draw-field [context {:keys [pos width height rgb]}]
  (-> context
      (set-fill-style rgb)
      (draw-filled-rectangle pos width height)))

(defn- render-board [context board]
  (doseq [field (apply concat board)]
    (draw-field context field))
  context)

(defn- draw-piece [context piece-img {:keys [pos width height]}]
  (.drawImage context piece-img (first pos) (second pos))
  context)

(defn- img-src [piece]
  (let [color (cond (chess/white? piece) "white"
                    (chess/black? piece) "black")]
    (when color
      (str "/images/" color "_" (.toLowerCase (name piece)) ".png"))))

(defn- load-piece-img [piece on-loaded-fn]
  (let [image (js/Image.)]
    (set! (.-onload image) #(on-loaded-fn image))
    (set! (.-src image) (img-src piece))))

(defn- render-piece [context field]
  (when (chess/piece? (:piece field))
    (load-piece-img (:piece field) (fn [img]
                                     (draw-piece context img field)))))

(defn- render-pieces [context board]
  (doseq [field (apply concat board)]
    (render-piece context field)))

(defn- add-field-marker [context {:keys [pos width height]} rgb]
  (set-stroke-style context rgb)
  (doseq [i (range 1 3)]
    (draw-rectangle-outline context
                            (map #(+ % i) pos)
                            (- width (* 2 i))
                            (- height (* 2 i)))))

(defn- remove-field-marker [context {:keys [pos width height rgb] :as field}]
  ;; TODO
  (add-field-marker context field rgb)
  (add-field-marker context field rgb)
  (add-field-marker context field rgb)
  (add-field-marker context field rgb))

(defn draw [context instructions]
  (doseq [instr instructions]
    (when (not= instr nil)
      (instr context))))

(defn render-board-instr [board]
  (fn [context]
    (render-board context board)))

(defn render-pieces-instr [board]
  (fn [context]
    (render-pieces context board)))

(defn remove-field-marker-instr [field]
  (when field
    (fn [context]
      (remove-field-marker context field))))

(defn add-field-marker-instr [field rgb]
  (when field
    (fn [context]
      (add-field-marker context field rgb))))

(defn remove-piece-instr [field]
  (fn [context]
    (draw-field context field)))

(defn add-piece-instr [field piece]
  (fn [context]
    (render-piece context (assoc field :piece piece))))