(ns chess.client.core
  (:require [chess.movelogic.vector2d.core :as chess]
            [chess.client.drawing :as d]
            [chess.client.remote :as r]
            [one.dispatch :as dispatch]))

(defn log [& stuff]
  (doseq [s stuff]
    (.log js/console s)))

(def BOARD-SIZE 400)

(defn piece-at [state [x y]]
  (get-in state [:board y x]))

(defn moving? [state]
  (contains? state :moving))

(defn moving-and-field-type
  "creates vector [<:moving/:not-moving> <:empty-field/:enemy-field/:player-field/:off-board>]"
  [state pos]
  [(if (moving? state)
     :moving
     :not-moving)
   (let [piece (piece-at state pos)
         piece-color (cond
                      (chess/white? piece) :w
                      (chess/black? piece) :b
                      :else nil)
         player-color (:turn state)]
     (cond
      (nil? piece) :off-board
      (nil? piece-color) :empty-field
      (= piece-color player-color) :player-field
      :else :enemy-field))])

(defn set-marker [state pos marker]
  (assoc-in state [:markers marker] pos))

(defn remove-marker [state marker]
  (update-in state [:markers] dissoc marker))

(defn possible-move [state pos]
  (-> state
      (set-marker pos :possible-move)
      (remove-marker :possible-capture)))

(defn possible-capture [state pos]
  (-> state
      (set-marker pos :possible-capture)
      (remove-marker :possible-move)))

(defn start-moving [state from-pos]
  (assoc state :moving {:from from-pos}))

(defn stop-moving [state]
  (-> state
      (dissoc :moving)
      (remove-marker :possible-move)
      (remove-marker :possible-capture)))

(defn move-from-pos [state]
  (get-in state [:moving :from]))

(defn chess-pos [[x y]]
  [x (- 7 y)])

(defn block-input [state]
  (assoc state :blocked true))

(defn unblock-input [state]
  (assoc state :blocked false))

(defn input-blocked? [state]
  (:blocked state))

(defn fire-async
  "Dispatches an event asynchronously by using a timeout of 0 ms.
   The code that actually dispatches the event is scheduled to run 'next', after the code that calls this function
   has been executed (if my understanding of JavaScript is correct...)."
  ([event-id]
     (fire-async event-id nil))
  ([event-id data]
     (js/setTimeout
      (fn []
        (if data
          (dispatch/fire event-id data)
          (dispatch/fire event-id)))
      0)))

(defn move [state from to]
  (fire-async :move {:from (chess-pos from)
                     :to (chess-pos to)})
  (-> state
      stop-moving
      (set-marker to :field-focus)
      block-input))

(defmulti handle-mouse-down moving-and-field-type)

(defmethod handle-mouse-down :default
  [state pos]
  state)

(defmethod handle-mouse-down [:not-moving :player-field]
  [state pos]
  (start-moving state pos))

(defmethod handle-mouse-down [:moving :player-field]
  [state pos]
  (-> state
      stop-moving
      (set-marker pos :field-focus)))

(defmethod handle-mouse-down [:moving :empty-field]
  [state pos]
  (move state (move-from-pos state) pos))

(defmethod handle-mouse-down [:moving :enemy-field]
  [state pos]
  (move state (move-from-pos state) pos))

(defmulti handle-mouse-move moving-and-field-type)

(defmethod handle-mouse-move :default
  [state pos]
  (set-marker state pos :field-focus))

(defmethod handle-mouse-move [:not-moving :off-board]
  [state pos]
  (remove-marker state :field-focus))

(defmethod handle-mouse-move [:moving :off-board]
  [state pos]
  (-> state
      (remove-marker :possible-move)
      (remove-marker :possible-capture)))

(defmethod handle-mouse-move [:moving :empty-field]
  [state pos]
  (possible-move state pos))

(defmethod handle-mouse-move [:moving :enemy-field]
  [state pos]
  (possible-capture state pos))

(defmethod handle-mouse-move [:moving :player-field]
  [state pos]
  (-> state
      (remove-marker :possible-move)
      (remove-marker :possible-capture)))

(defn create-canvas [width height]
  (doto (.createElement js/document "canvas")
    (.setAttribute "width" width)
    (.setAttribute "height" height)))

(defn board-elem []
  (-> js/document
      (.getElementById "chess-board")))

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      game-id (.getAttribute (board-elem) "data-game-id")
      context (.getContext canvas "2d")
      state (atom nil)]
  (r/get-gamestate game-id
                 (fn [gamestate]
                   (-> (board-elem)
                       (.appendChild canvas))
                   (d/listen-for-mouse-move context (fn [pos]
                                                      (swap! state (fn [current-state]
                                                                     (if-not (input-blocked? current-state)
                                                                       (handle-mouse-move current-state pos)
                                                                       current-state)))))
                   (d/listen-for-mouse-down context (fn [pos]
                                                      (swap! state (fn [current-state]
                                                                     (if-not (input-blocked? current-state)
                                                                       (handle-mouse-down current-state pos)
                                                                       current-state)))))
                   (add-watch state :draw-board (fn [k r o n]
                                                  (d/draw o n context)))
                   (dispatch/react-to #{:move} (fn [_ move]
                                                 (r/move game-id move
                                                         (fn [gamestate-after-move]
                                                           (if gamestate-after-move
                                                             (do
                                                               (reset! state gamestate-after-move)
                                                               (fire-async :next))
                                                             (do
                                                               (js/alert "move not possible")
                                                               (swap! state unblock-input)))))))
                   (dispatch/react-to #{:next} (fn [_ _]
                                                 (r/nxt game-id
                                                        (fn [gamestate-after-next]
                                                          (reset! state gamestate-after-next)))))
                   (reset! state gamestate))))