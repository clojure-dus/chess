(defproject chess "1.0.0-SNAPSHOT"
  :description "chess engine"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [ring "1.1.0"] ; http basics
                 [compojure "1.1.0"] ; http routing
                 [hiccup "1.0.1"] ; generating html
                 [org.clojure/data.json "0.2.0"]
                 [com.typesafe.akka/akka-actor "2.0.3"]
                 [com.typesafe.akka/akka-remote "2.0.3"]
                 [com.typesafe.akka/akka-kernel "2.0.3"]]
  :repositories {"Typesafe Repository for Akka" "http://repo.typesafe.com/typesafe/releases/"}
  :plugins [[lein-cljsbuild "0.2.9"]] ; see https://github.com/emezeske/lein-cljsbuild
  :cljsbuild {:builds [{:source-path "src-cljs"
                        :compiler {:output-to "resources/public/js/chess.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]
              :crossovers [chess.core]
              :crossover-path "crossover-cljs"}
  :aot [chess.util.actors]
  :warn-on-reflection false
  :jvm-opts ["-Xmx1024M" "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  :main chess.web
  :java-source-paths ["src/chess/bitboard/impl"]
  ;:extra-classpath-dirs ["/usr/lib/jvm/java-6-sun/lib/tools.jar"]
  )
