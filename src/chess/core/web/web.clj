(ns chess.web
  (:require [clojure.data.json :as json])
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.util.response :only [response content-type redirect]]
        [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [ring.middleware.reload :only [wrap-reload]]
        [compojure.core :only [defroutes GET POST]]
        [compojure.handler :only [api]]
        [hiccup.page :only [html5]]
        [hiccup.form :only [form-to submit-button]]
        [chess.movelogic.move-generator :only [initial-board change-turn move-piece]]
        [chess.ai.move-selection.min-max :only [select-move]]))

(defonce games (atom {}))

(defn create-game []
  (let [id (str (java.util.UUID/randomUUID))]
    (swap! games assoc id (initial-board))
    id))

(defn page [& body]
  (list
   [:head
    [:link {:rel "stylesheet" :type "text/css" :href "/css/chess.css"}]
    [:title "chess"]]
   [:body body]))

(def index-page
  (page
   (form-to {:id "new-game-form"}
            [:post "/games"]
            (submit-button "new game"))
   [:script "document.getElementById('new-game-form').submit();"]))

(defn game-page [game-id]
  (page
   [:div#chess-board {:data-game-id game-id}]
   [:script {:src "/js/chess.js" :type "text/javascript"}]))

(defn show-html [content]
  (-> (response (html5 content))
      (content-type "text/html")))

(defn json [m]
  (-> (json/write-str m)
      response
      (content-type "application/json")))

(defn bad-request [msg]
  (-> msg
      response
      (assoc :status 400)))

(defroutes chess
  (GET "/" []
       (show-html index-page))
  (POST "/games" []
        (redirect (str "/games/" (create-game))))
  (GET "/games/:id" [id]
       (when (contains? @games id)
         (show-html (game-page id))))
  (GET "/gamestates/:id" [id]
       (when-let [gamestate (get @games id)]
         (json gamestate)))
  (POST "/gamestates/:id/move" {json-stream :body
                                {:keys [id]} :params
                                :as request}
        (when-let [gamestate (get @games id)]
          (let [{:keys [from to]} (json/read-str (slurp json-stream) :key-fn keyword)]
            (if-let [gamestate-after-move (move-piece gamestate from to)]
              (let [new-gamestate (change-turn gamestate-after-move)]
                (swap! games assoc id new-gamestate)
                (json new-gamestate))
              (bad-request "move not allowed")))))
  (POST "/gamestates/:id/next" [id]
        (when-let [gamestate (get @games id)]
          (let [[from to] (select-move gamestate)
                new-gamestate (-> gamestate
                                  (make-move from to)
                                  change-turn)]
            (do
              (swap! games assoc id new-gamestate)
              (json new-gamestate))))))

(def webapp 
  (-> chess
      (wrap-reload ["src"])
      (wrap-resource "public")
      wrap-file-info))

(defn -main [& args]
  (run-jetty (api #'webapp) {:port 8080 :join? true}))