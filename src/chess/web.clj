(ns chess.web
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.util.response :only [response content-type redirect]]
        [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [ring.middleware.reload :only [wrap-reload]]
        [compojure.core :only [defroutes GET POST]]
        [compojure.handler :only [api]]
        [hiccup.page :only [html5]]
        [hiccup.form :only [form-to submit-button]]
        [chess.core :only [initial-board]]))

(def games (atom {}))

(defn create-game []
  (let [id (str (java.util.UUID/randomUUID))]
    (swap! games assoc id initial-board)
    id))

(defn page [& body]
  (list
   [:head
    [:link {:rel "stylesheet" :type "text/css" :href "/css/chess.css"}]
    [:title "chess"]]
   [:body body]))

(def index-page
  (page
   (form-to [:post "/games"]
            (submit-button "new game"))))

(defn game-page [game-id]
  (page
   [:div#chess-board {:data-game-id game-id}]
   [:script {:src "/js/chess.js" :type "text/javascript"}]))

(defn show-html [content]
  (-> (response (html5 content))
      (content-type "text/html")))

(defroutes chess
  (GET "/" []
       (show-html index-page))
  (POST "/games" []
        (redirect (str "/games/" (create-game))))
  (GET "/games/:id" [id]
       (when (contains? @games id)
         (show-html (game-page id)))))

(def webapp 
  (-> chess
      (wrap-reload ["src"])
      (wrap-resource "public")
      wrap-file-info))

(defn -main [& args]
  (run-jetty (api #'webapp) {:port 8080 :join? true}))