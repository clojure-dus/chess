(ns chess.util.actors
  (:import [akka.actor Props UntypedActor UntypedActorFactory ActorSystem]
           [com.typesafe.config ConfigFactory]
           [akka.routing RoundRobinRouter]))

(def ^:dynamic *actor-context* nil)

(defn create-context [actor]
  {:self (.getSelf actor)
   :sender (.getSender actor)
   :context (.getContext actor)})

(defn self []
  (:self *actor-context*))

(defn sender []
  (:sender *actor-context*))

(defn context []
  (:context *actor-context*))

(defn stop []
  (.stop (context) (self)))

(defn reply [msg]
  (.tell (sender) msg))

(defn shutdown []
  (-> (context)
      .system
      .shutdown))

(defn make-props [handlers]
  (Props. (proxy [UntypedActorFactory] []
            (create []
              (proxy [UntypedActor] []
                (onReceive [[type & args :as msg]]
                  (if-let [handler (get handlers type)]
                    (binding [*actor-context* (create-context this)]
                      (apply handler args))
                    (println "unknown type of message:" msg))))))))

(defn with-router [props router]
  (if router
    (.withRouter props router)
    props))

(defn make-system []
  (ActorSystem/create
   "name"
   (ConfigFactory/load
    (ConfigFactory/parseResourcesAnySyntax "doesntexist"))))

(defn make-actor
  ([handlers]
     (make-actor nil handlers))
  ([{:keys [router system] :or {system (context)}}
    handlers]
     (let [props (-> (make-props handlers)
                     (with-router router))
           actor (.actorOf system props)]
       (fn this
         ([msg] (this msg (self)))
         ([msg sender] (if sender
                         (.tell actor msg sender)
                         (.tell actor msg)))))))

(comment Pi Example
;; see https://github.com/gaverhae/okku-pi
(defn calculate-pi-for [^long st ^long n]
  (let [limit (* (inc st) n)]
    (loop [i (* st n) tot 0.0]
      (if (= i limit)
        tot
        (recur (unchecked-inc i) (+ tot
                                    (* 4.0 (/ (double (unchecked-add 1 (unchecked-negate (unchecked-multiply 2 (unchecked-remainder-int i 2)))))
                                              (double (unchecked-add 1 (unchecked-multiply 2 i)))))))))))

(defn make-worker-router [system workers-count]
  (make-actor
   {:router (RoundRobinRouter. workers-count)
    :system system}
   {:work (fn [start n-elem]
            (reply [:result (calculate-pi-for start n-elem)]))}))

(defn make-master [system workers-count messages-count elem-count result-listener]
  (let [workers (make-worker-router system workers-count)
        res (atom {:pi 0 :nr 0})
        start (System/currentTimeMillis)]
    (make-actor
     {:system system}
     {:compute (fn []
                 (dotimes [n messages-count]
                   (workers [:work n elem-count])))
      :result (fn [value]
                (swap! res #(merge-with + % {:pi value :nr 1}))
                (when (= (:nr @res) messages-count)
                  (result-listener [:approx (:pi @res) (- (System/currentTimeMillis) start)])
                  (stop)))})))

(defn make-listener [system]
  (make-actor
   {:system system}
   {:approx (fn [pi dur]
              (printf "\n\tPi approximation: \t\t%1.8f\n\tCalculation time: \t%8d millis" pi dur)
              (shutdown))}))

(defn run-pi []
  (let [workers 4
        elements 10000
        messages 10000
        system (make-system)
        listener (make-listener system)
        master (make-master system workers messages elements listener)]
    (master [:compute])
    (.awaitTermination system)))
)

(defn make-routed-workers [system results-receiver f workers-count]
  (make-actor
   {:router (RoundRobinRouter. workers-count)
    :system system}
   {:compute (fn [idx item]
               (println "computed: " (f item))
               (results-receiver [:result idx (f item)]))}))

(defn make-results-receiver [system result-promise expected-results-count]
  (let [received-results (atom [])]
    (add-watch received-results :deliver-when-done
               (fn [k r o n]
                 (println "received results:" n)
                 (when (= (count n) expected-results-count)
                   (deliver result-promise
                            (->> n
                                 (sort-by first)
                                 (map second))))))
    (make-actor
     {:system system}
     {:result (fn [idx result]
                (println "received result" result)
                (swap! received-results conj [idx result]))})))

(defn actor-map [system f coll]
  (let [result (promise)
        results-receiver (make-results-receiver system result (count coll))
        workers (make-routed-workers system results-receiver f (count coll))
        indexed-coll (map vector (range) coll)]
    (doseq [[idx item] indexed-coll]
      (workers [:compute idx item]))
    ;; TODO shutdown actors?
    @result))