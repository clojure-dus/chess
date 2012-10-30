(ns chess.client.remote
  (:require [goog.net.XhrIo]))

(defn- keywordize-rochade-vec [state]
  (update-in state [:rochade] #(vec (map keyword %))))

(defn- keywordize-board-vec [state]
  (update-in state [:board] #(vec (map (fn [row]
                                         (vec (map keyword row)))
                                       %))))

(defn- keywordize-turn [state]
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