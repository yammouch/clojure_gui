(ns Gui)

(import '[java.io IOException])
(import '[javafx.application Application])
(import '[javafx.fxml FXMLLoader])
(import '[javafx.scene Scene])
(import '[javafx.stage Stage])

;(require 'clojure.pprint)

(gen-class
  :name "Gui"
  :main true
  :extends javafx.application.Application)

(defn -start [this ^Stage stage]
  ;(clojure.pprint/pprint (. (. this getClass)
  ;                          getResource "gui.fxml"))
  (try
    (let [root (FXMLLoader/load (. (. this getClass)
                                   getResource "gui.fxml"))
          scene (Scene. root 200 100)]
      (.setScene stage scene)
      (.show stage))
    (catch IOException e (.printStackTrace e))
    ))

(defn -main [& args]
  (Application/launch (Class/forName "Gui") (into-array String [])))
