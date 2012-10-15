(ns chess.client.core
  (:require [goog.net.XhrIo]
            [chess.core :as chess]
            [chess.client.drawing :as d]))

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
                      (chess/white? piece) :white
                      (chess/black? piece) :black
                      :else nil)
         player-color (:player-color state)]
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

(defn swap-player [state]
  (let [current (:player-color state)
        next (condp = current
               :white :black
               :black :white)]
    (assoc state :player-color next)))

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
  (-> state
      stop-moving
      (set-marker pos :field-focus)
      (chess/move-piece (chess-pos (move-from-pos state))
                        (chess-pos pos))
      swap-player))

(defmethod handle-mouse-down [:moving :enemy-field]
  [state pos]
  (-> state
      stop-moving
      (set-marker pos :field-focus)
      (chess/move-piece (chess-pos (move-from-pos state))
                        (chess-pos pos))
      swap-player))

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

(defn keywordize-rochade-vec [state]
  (update-in state [:rochade] #(vec (map keyword %))))

(defn keywordize-board-vec [state]
  (update-in state [:board] #(vec (map (fn [row]
                                         (vec (map keyword row)))
                                       %))))

(defn keywordize-turn [state]
  (update-in state [:turn] keyword))

(defn get-gamestate [id callback]
  (let [url (str "/gamestates/" id)]
    (.send goog.net.XhrIo url (fn [response]
                                (-> response
                                    .-target
                                    .getResponseJson
                                    (js->clj :keywordize-keys true)
                                    keywordize-rochade-vec
                                    keywordize-board-vec
                                    keywordize-turn
                                    callback)))))

(let [canvas (create-canvas BOARD-SIZE BOARD-SIZE)
      game-id (.getAttribute (board-elem) "data-game-id")
      context (.getContext canvas "2d")
      state (atom nil)]
  (get-gamestate game-id
                 (fn [gamestate]
                   (-> (board-elem)
                       (.appendChild canvas))
                   (d/listen-for-mouse-move context (fn [pos]
                                                      (swap! state handle-mouse-move pos)))
                   (d/listen-for-mouse-down context (fn [pos]
                                                      (swap! state handle-mouse-down pos)))
                   (add-watch state :draw-board (fn [k r o n]
                                                  (d/draw o n context)))
                   (reset! state (-> gamestate
                                     (assoc :player-color :white))))))