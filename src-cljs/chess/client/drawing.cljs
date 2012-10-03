(ns chess.client.drawing
  (:require [chess.core :as chess]))

(defn- log [& stuff]
  (doseq [s stuff]
    (.log js/console s)))

(def BOARD-SIZE 400)

(defn- rgb [r g b]
  [r g b])

(def colors {:light-field (rgb 255 255 255)
             :dark-field (rgb 173 173 173)
             :field-focus (rgb 80 80 80)
             :possible-move (rgb 0 255 0)
             :possible-capture (rgb 255 0 0)})

(defn- pos [x-from-upper-left y-from-upper-left]
  [x-from-upper-left y-from-upper-left])

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

(defn- covers? [{:keys [pos width height]} [x y]]
  (let [[field-x field-y] pos]
    (and (<= field-x x (+ field-x width))
         (<= field-y y (+ field-y height)))))

(defn- field-at [board pos]
  (let [fields (apply concat board)]
    (some #(when (covers? % pos)
             %)
          fields)))

(defn- map-alternating [coll & fs]
  (map #(%1 %2)
       (cycle fs)
       coll))

(defn- light-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:light-field colors))
   #(assoc % :rgb (:dark-field colors))))

(defn- dark-row [fields]
  (map-alternating fields
   #(assoc % :rgb (:dark-field colors))
   #(assoc % :rgb (:light-field colors))))

(defn- put-pieces
  "fields: vector of vectors with fields
   pieces: vector of vectors with pieces"
  [fields pieces]
  (map (fn [field-row piece-row]
         (map (fn [field piece]
                (assoc field :piece piece))
              field-row
              piece-row))
       fields
       pieces))

(defn- chessboard-rows [board-size]
  (let [field-size (/ board-size 8)
        steps (range 0 board-size field-size)
        positions (for [y steps x steps] (pos x y))
        fields (map (fn [pos]
                      {:pos pos
                       :width field-size
                       :height field-size})
                    positions)]
    (map-alternating (partition 8 fields) light-row dark-row)))

(defn- mouse-pos-relative-to-canvas [event canvas]
  "see http://miloq.blogspot.de/2011/05/coordinates-mouse-click-canvas.html"
  (let [[x y] (if (and (not= (.-x event) js/undefined)
                       (not= (.-x event) js/undefined))
                ;; "normal" way to get position
                [(.-x event)
                 (.-y event)]
                ;; for firefox
                [(+ (.-clientX event) (-> js/document .-body .-scrollLeft) (-> js/document .-documentElement .-scrollLeft))
                 (+ (.-clientY event) (-> js/document .-body .-scrollTop) (-> js/document .-documentElement .-scrollTop))])]
    (pos (- x (.-offsetLeft canvas))
         (- y (.-offsetTop canvas)))))

(defn- coordinates-to-pos [coordinates]
  (let [field-size (/ BOARD-SIZE 8)]
    (map #(+ (/ field-size 2) (* field-size %)) coordinates)))

(defn- pos-to-coordinates [position]
  (let [field-size (/ BOARD-SIZE 8)]
    (map #(Math/floor (/ % field-size)) position)))

(defn- render-marker [context board marker-type coordinates]
  (let [color (get colors marker-type)
        pos (coordinates-to-pos coordinates)
        field (field-at board pos)]
    (when field
      (add-field-marker context field color))))

(defn- render-markers [context board markers]
  (doseq [marker markers]
    (apply render-marker context board marker)))

(defn- remove-markers [context board]
  (doseq [field (apply concat board)]
    (remove-field-marker context field)))

(defn- changed-board? [old-state new-state]
  (not= (:board old-state) (:board new-state)))

(defn- changed-markers? [old-state new-state]
  (not= (:markers old-state) (:markers new-state)))

(defn- listen-for [event-type context callback]
  (let [canvas (.-canvas context)]
    (.addEventListener canvas
                       event-type
                       (fn [event]
                         (callback (-> event
                                       (mouse-pos-relative-to-canvas canvas)
                                       pos-to-coordinates)))
                       false)))

(def listen-for-mouse-move (partial listen-for "mousemove"))

(def listen-for-mouse-down (partial listen-for "mousedown"))

(let [fields (chessboard-rows BOARD-SIZE)]
  (defn draw [old-state new-state context]
    (let [pieces (:board new-state)
          board (put-pieces fields pieces)]
      (cond (changed-board? old-state new-state)
            (do
              (clear-rectangle context (pos 0 0) BOARD-SIZE BOARD-SIZE)
              (render-board context board)
              (render-pieces context board)
              (render-markers context board (:markers new-state)))
            (changed-markers? old-state new-state)
            (do
              (remove-markers context board)
              (render-markers context board (:markers new-state)))))))