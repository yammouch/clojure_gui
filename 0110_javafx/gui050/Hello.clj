(ns Hello)

(gen-class
  :name "Hello"
  ;:main true
  :extends javafx.application.Application)

(import '[javafx.application Application])

(defn -start [self stage]
  (println "hoge"))

(defn -main []
  (println (read-line)) ; This does not work.
  (Application/launch (into-array String [])))
