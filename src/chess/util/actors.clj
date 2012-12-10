(ns chess.util.actors
  (:import [akka.actor Props UntypedActor ActorContext UntypedActorFactory ActorSystem PoisonPill ActorRef]
           [com.typesafe.config ConfigFactory]
           [akka.routing RoundRobinRouter]))

(def ^:dynamic *actor-context* nil)

(defn create-context [^UntypedActor actor]
  {:self (.getSelf actor)
   :sender (.getSender actor)
   :context (.getContext actor)})

(defn ^ActorRef self []
  (:self *actor-context*))

(defn ^ActorRef sender []
  (:sender *actor-context*))

(defn ^ActorContext context []
  (:context *actor-context*))

;; see http://kotka.de/blog/2010/02/gen-class_how_it_works_and_how_to_use_it.html
(gen-class
 :name chess.util.actors.GenericActor
 :extends akka.actor.UntypedActor
 :state handlers
 :init init
 :constructors {[java.util.Map] []}
 :prefix genericActor-)

(defn genericActor-init [handlers]
  [[] handlers])

(defn genericActor-toString [this]
  (str "GenericActor with handlers " (.handlers this)))

(defn genericActor-onReceive [this [type & args :as msg]]
  (try
    (if-let [handler (get (.handlers this) type)]
      (binding [*actor-context* (create-context this)]
        (apply handler args))
      (println "unknown type of message:" msg))
    (catch Exception e
      (.printStackTrace e))))

(defn make-props [handlers]
  (Props.
   (reify UntypedActorFactory
     (create [this]
       (chess.util.actors.GenericActor. handlers)))))

(defn with-router [^Props props router]
  (if router
    (.withRouter props router)
    props))

(defn make-actor
  ([handlers]
     (make-actor nil handlers))
  ([{:keys [router system] :or {system (context)}}
    handlers]
     (let [props (-> (make-props handlers)
                     (with-router router))
           actor (.actorOf ^ActorContext system props)]
       (fn this
         ([msg] (this msg (self)))
         ([msg sender] (if sender
                         (.tell actor msg sender)
                         (.tell actor msg)))))))

(defn stop []
  (.stop (context) (self)))

(defn reply [msg]
  (.tell (sender) msg))

(defn shutdown []
  (-> (context)
      .system
      .shutdown))

(defn make-system []
  (ActorSystem/create
   "name"
   (ConfigFactory/load
    (ConfigFactory/parseResourcesAnySyntax "doesntexist"))))

(defn poison-pill
  ([]
     (PoisonPill/getInstance))
  ([actor]
     (actor (poison-pill))))

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
   {:compute (fn [idx chunk]
                     ;;(println "compute called for chunk" chunk)
                     (results-receiver [:result idx (map f chunk)]))}))

(defn make-results-receiver [system result-promise expected-results-count]
  (let [received-results (atom [])]
    (make-actor
     {:system system}
     {:result (fn [idx result]
                ;;(println "received result" result)
                (swap! received-results conj [idx result])
                (when (= (count @received-results) expected-results-count)
                  (deliver result-promise
                           (->> @received-results
                                (sort-by first)
                                (map second)
                                (apply concat)))))})))

(defn eager-actor-map
  "Applies f to every item in coll and returns a seq of the results (like map).
  The results are computed by actors which are created in system. The computation
  of the results is done in parallel but not semi-lazy (this is different to pmap)."
  [system f coll]
  (let [result (promise)
        worker-count 10
        chunk-size (int (/ (count coll) worker-count))
        chunks (partition chunk-size chunk-size () coll)
        results-receiver (make-results-receiver system result (count chunks))
        workers (make-routed-workers system results-receiver f worker-count)
        indexed-chunks (map vector (range) chunks)]
    (doseq [[idx chunk] indexed-chunks]
      (workers [:compute idx chunk]))
    (let [the-result @result]
      (poison-pill results-receiver)
      (poison-pill workers)
      the-result)))