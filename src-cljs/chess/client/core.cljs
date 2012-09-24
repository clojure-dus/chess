(ns chess.client.core
  (:require [chess.core :as chess]
            [chess.client.drawing :as d]))

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
             :field-focus (rgb 80 80 80)
             :possible-move (rgb 0 255 0)
             :possible-capture (rgb 255 0 0)})

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

(defn add-drawing-instrs [state drawing-instrs]
  (let [instrs (get state :drawing-instructions [])]
    (if drawing-instrs
      (assoc state :drawing-instructions (into instrs drawing-instrs))
      state)))

(defn draw! [state context]
  (when-let [instrs (:drawing-instructions state)]
    (d/draw context instrs)))

(defn mark-drawn [state]
  (dissoc state :drawing-instructions))

(defn execute-drawing-instructions [state context]
  (draw! state context)
  (mark-drawn state))

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
      (nil? piece) :off-board
      (nil? piece-color) :empty-field
      (= piece-color player-color) :player-field
      :else :enemy-field))])

(defn focus-field [state field]
  (-> state
      (add-drawing-instrs [(d/remove-field-marker-instr (:focused-field state))
                           (d/add-field-marker-instr field (:field-focus colors))])
      (assoc :focused-field field)))

(defn start-moving [state from-field]
  (assoc state :moving {:from from-field}))

(defn stop-moving [state]
  (-> state
      (dissoc :moving)
      (add-drawing-instrs [(d/remove-field-marker-instr (get-in state [:moving :to]))])))

(defn move-piece [state from-field to-field]
  (-> state
      ;; TODO instructions for server call (which will update board in state?)
      (add-drawing-instrs [(d/remove-piece-instr from-field)
                           (d/remove-piece-instr to-field)
                           (d/add-piece-instr to-field (:piece from-field))])))

(defn make-move [state from-field to-field]
  (-> state
      stop-moving
      (move-piece from-field to-field)
      (focus-field to-field)))

(defn describe-mouse-move [state pos]
  (let [from-field (:focused-field state)
        to-field (field-at state pos)
        description (cond
                     (= from-field to-field) :focus-not-changed
                     (and from-field to-field) :focus-changed
                     from-field :board-exit
                     to-field :board-enter)]
    [from-field to-field description]))

(defn track-field-focus [state pos color]
  (let [[from to desc] (describe-mouse-move state pos)
        drawing-instrs (case desc
                         :focus-changed [(d/remove-field-marker-instr from)
                                         (d/add-field-marker-instr to color)]
                         :board-exit [(d/remove-field-marker-instr from)]
                         :board-enter [(d/add-field-marker-instr to color)]
                         nil)]
    (-> state
        (add-drawing-instrs drawing-instrs)
        (assoc :focused-field to))))

(defn remove-move-target [state]
  (let [move-target (get-in state [:moving :to])]
    (-> state
        (update-in [:moving] dissoc :to)
        (add-drawing-instrs [(d/remove-field-marker-instr move-target)]))))

(defn set-move-target [state field color]
  (let [prev-move-target (get-in state [:moving :to])]
    (-> state
        (assoc-in [:moving :to] field)
        (add-drawing-instrs [(d/remove-field-marker-instr prev-move-target)
                             (d/add-field-marker-instr field color)]))))

(defmulti handle-mouse-down moving-and-field-type)

(defmethod handle-mouse-down :default
  [state pos]
  state)

(defmethod handle-mouse-down [:not-moving :player-field]
  [state pos]
  (start-moving state (field-at state pos)))

(defmethod handle-mouse-down [:moving :player-field]
  [state pos]
  (-> state
      stop-moving
      (focus-field (field-at state pos))))

(defmethod handle-mouse-down [:moving :empty-field]
  [state pos]
  (make-move state (get-in state [:moving :from]) (field-at state pos)))

(defmethod handle-mouse-down [:moving :enemy-field]
  [state pos]
  (make-move state (get-in state [:moving :from]) (field-at state pos)))

(defn canvas-mousedown-handler [context event]
  (update-state (fn [state]
                  (-> state
                      (handle-mouse-down (mouse-pos-relative-to-canvas event (.-canvas context)))
                      (execute-drawing-instructions context)))))

(defmulti handle-mouse-move moving-and-field-type)

(defmethod handle-mouse-move :default
  [state pos]
  (track-field-focus state pos (:field-focus colors)))

(defmethod handle-mouse-move [:moving :off-board]
  [state pos]
  (remove-move-target state))

(defmethod handle-mouse-move [:moving :empty-field]
  [state pos]
  (set-move-target state (field-at state pos) (:possible-move colors)))

(defmethod handle-mouse-move [:moving :enemy-field]
  [state pos]
  (set-move-target state (field-at state pos) (:possible-capture colors)))

(defmethod handle-mouse-move [:moving :player-field]
  [state pos]
  (remove-move-target state))

(defn canvas-mousemove-handler [context event]
  (update-state (fn [state]
                  (-> state
                      (handle-mouse-move (mouse-pos-relative-to-canvas event (.-canvas context)))
                      (execute-drawing-instructions context)))))

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
                        fields (chessboard-rows BOARD-SIZE)
                        board (put-pieces fields pieces)]
                    (-> (assoc state
                          :player-color :white
                          :board board)
                        (add-drawing-instrs [(d/render-board-instr board)
                                             (d/render-pieces-instr board)])
                        (execute-drawing-instructions context)))))
  (.addEventListener canvas "mousemove" (partial canvas-mousemove-handler context) false)
  (.addEventListener canvas "mousedown" (partial canvas-mousedown-handler context) false))