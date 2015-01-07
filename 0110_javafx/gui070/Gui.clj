(ns Gui)

(import
  '(java.io IOException)
  '(javafx.application Application)
  '(javafx.fxml FXMLLoader)
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
          scene (Scene. root 200 100)]
      (.setScene stage scene)
      (.show stage))
    (catch IOException e (.printStackTrace e))
    ))

(defn -main [& args]
  (Application/launch (Class/forName "Gui") (into-array String [])))
