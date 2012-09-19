(ns chess.client.core
  (:require ;;[clojure.browser.repl :as repl]
            [one.dispatch :as disp]
            [chess.core :as chess]))

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

(defn chessboard-rows [board-size]
  (let [field-size (/ board-size 8)
        steps (range 0 board-size field-size)
        positions (for [y steps x steps] (pos x y))
        fields (map (fn [pos]
                      {:pos pos
                       :width field-size
                       :height field-size})
                    positions)]
    (map-alternating (partition 8 fields) light-row dark-row)))

(defn rgb-string [[r g b]]
  (str "rgb(" r "," g "," b ")"))

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

(defn draw-board-on [context state]
  (let [board-rows (:board state)]
    (doseq [field (apply concat board-rows)]
      (draw-field context field))
    context))

(defn create-canvas [width height]
  (doto (.createElement js/document "canvas")
    (.setAttribute "width" width)
    (.setAttribute "height" height)))

(defn mouse-pos-relative-to-canvas [event canvas]
  "see http://miloq.blogspot.de/2011/05/coordinates-mouse-click-canvas.html"
  (comment (if (or (.-layerX event) (= 0 (.-layerX event)))
    (pos (.-layerX event) (.-layerY event))
    (pos (.-offsetX event) (.-offsetY event))))
  (let [[x y] (if (and (not= (.-x event) js/undefined)
                       (not= (.-x event) js/undefined))
                [(.-x event)
                 (.-y event)] ;; "normal" way to get position
                [(+ (.-clientX event) (-> js/document .-body .-scrollLeft) (-> js/document .-documentElement .-scrollLeft))
                 (+ (.-clientY event) (-> js/document .-body .-scrollTop) (-> js/document .-documentElement .-scrollTop))])] ;; for firefox
    (pos (- x (.-offsetLeft canvas))
         (- y (.-offsetTop canvas)))))

(defn covers? [{:keys [pos width height]} [x y]]
  (let [[field-x field-y] pos]
    (and (<= field-x x (+ field-x width))
         (<= field-y y (+ field-y height)))))

(defn field-at [state pos]
  (let [fields (apply concat (:board state))]
    (some #(when (covers? % pos)
             %)
          fields)))

(defn add-field-marker [context {:keys [pos width height]} rgb]
  (set-stroke-style context rgb)
  (doseq [i (range 1 3)]
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

(defn moving-and-field-type
  "creates vector [<:moving/:not-moving> <:empty-field/:enemy-field/:player-field>]"
  [state pos & _]
  [(if (:moving state)
     :moving
     :not-moving)
   (let [piece (:piece (field-at state pos))
         piece-color (cond
                      (chess/white? piece) :white
                      (chess/black? piece) :black
                      :else nil)
         player-color (:player-color state)]
     (cond
      (nil? piece-color) :empty-field
      (= piece-color player-color) :player-field
      :else :enemy-field))])

(defmulti handle-mouse-down moving-and-field-type)

(defmethod handle-mouse-down [:not-moving :empty-field]
  [state pos]
  state)

(defmethod handle-mouse-down [:not-moving :enemy-field]
  [state pos]
  state)

(defmethod handle-mouse-down [:not-moving :player-field]
  [state pos]
  (assoc state :moving {:from (field-at state pos)}))

(defmethod handle-mouse-down [:moving :player-field]
  [state pos]
  (let [from-field (-> state :moving :from)
        current-field (field-at state pos)]
    (if (= from-field current-field)
      (dissoc state :moving)
      state)))

(defn canvas-mousedown-handler [context event]
  (update-state (fn [state]
                  (let [new-state (handle-mouse-down state (mouse-pos-relative-to-canvas event (.-canvas context)))]
                    ;; TODO (draw (drawing-instructions new-state) context)
                    new-state))))

(defmulti handle-mouse-move moving-and-field-type)

(defn default-mouse-move [state pos context]
  (let [previous-field (:focused-field state)
        new-field (field-at state pos)]
    (when (not= previous-field new-field)
      (when previous-field
        (disp/fire :field-focus-lost context previous-field))
      (when new-field
        (disp/fire :field-focus context new-field)))
    (assoc state :focused-field new-field)))

(defmethod handle-mouse-move [:not-moving :empty-field]
  [state pos & [context]]
  (default-mouse-move state pos context))

(defmethod handle-mouse-move [:not-moving :enemy-field]
  [state pos & [context]]
  (default-mouse-move state pos context))

(defmethod handle-mouse-move [:not-moving :player-field]
  [state pos & [context]]
  (default-mouse-move state pos context))

(defn canvas-mousemove-handler [context event]
  (update-state (fn [state]
                  (let [new-state (handle-mouse-move state
                                                     (mouse-pos-relative-to-canvas event (.-canvas context))
                                                     context)]
                    new-state))))

(defn draw-piece [context piece-img {:keys [pos width height]}]
  (.drawImage context piece-img (first pos) (second pos))
  context)

(defn img-src [piece]
  (let [color (cond (chess/white? piece) "white"
                    (chess/black? piece) "black")]
    (when color
      (str "/images/" color "_" (.toLowerCase (name piece)) ".png"))))

(defn load-piece-img [piece on-loaded-fn]
  (let [image (js/Image.)]
    (set! (.-onload image) #(on-loaded-fn image))
    (set! (.-src image) (img-src piece))))

(defn render-pieces [context state]
  (let [fields (apply concat (:board state))]
    (doseq [field fields]
      (let [piece (:piece field)]
        (when (chess/piece? piece)
          (load-piece-img piece (fn [img]
                                  (draw-piece context img field))))))))

(defn put-pieces
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

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      context (.getContext canvas "2d")]
  (-> js/document
      (.getElementById "chess-board")
      (.appendChild canvas))
  (update-state (fn [state]
                  (let [pieces (:board chess/initial-board)
                        fields (chessboard-rows BOARD-SIZE)]
                  (assoc state
                    :player-color :white
                    :board (put-pieces fields pieces)))))
  (draw-board-on context (current-state))
  (render-pieces context (current-state))
  (.addEventListener canvas "mousemove" (partial canvas-mousemove-handler context) false)
  (.addEventListener canvas "mousedown" (partial canvas-mousedown-handler context) false))