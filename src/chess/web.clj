(ns chess.web
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.util.response :only [response content-type]]
        [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [compojure.core :only [defroutes GET]]
        [compojure.handler :only [api]]
        [hiccup.core :only [html]]))

(def client-page
  [:html
   [:head
    [:title "chess"]
    [:script {:src "/js/chess.js" :type "text/javascript"}]]
   [:body
    [:h1 "chess"]]])

(defroutes chess-client
  (GET "/chess" []
       (-> (response (html client-page))
           (content-type "text/html"))))

(def webapp
  (-> chess-client
      (wrap-resource "public")
      wrap-file-info))

(defonce server
  (run-jetty (api #'webapp) {:port 8080 :join? false}))