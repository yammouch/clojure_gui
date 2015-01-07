; reference:
; http://libro.tuyano.com/index3?id=12496003&page=4

(ns Gui)

(import
  '(java.io IOException)
  '(javafx.application Application)
  '(javafx.fxml FXMLLoader)
  '(javafx.event EventHandler)
  '(javafx.scene Scene)
  '(javafx.stage Stage))

(gen-class
  :name "Gui"
  :main true
  :extends javafx.application.Application)

(defn -start [this ^Stage stage]
  (try
    (let [root (FXMLLoader/load (. (. this getClass)
                                   getResource "gui.fxml"))
          scene (Scene. root 200 100)
          label1 (.getTop root)
          field1 (.getCenter root)
          button (.getBottom root)
          button-action (proxy [EventHandler] []
                          (handle [event]
                            (.setText label1
                                      (.getText field1))))]
      (.setOnAction button button-action)
      (.setScene stage scene)
      (.show stage))
    (catch IOException e (.printStackTrace e))
    ))

(defn -main [& args]
  (Application/launch (Class/forName "Gui") (into-array String [])))
