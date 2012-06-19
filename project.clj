(defproject demo-app "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[swank-clojure "1.4.0"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  :extra-classpath-dirs ["/usr/lib/jvm/java-6-sun/lib/tools.jar"])