(ns Hello)

(gen-class
  :name "Hello"
  :main true
  :extends javafx.application.Application)

(import '[javafx.application Application])

(defn -start [self stage]
  (println "hoge"))

(defn -main [& args]
  (println (read-line)) ; This does not work.
  (Application/launch (Class/forName "Hello") (into-array String [])))
