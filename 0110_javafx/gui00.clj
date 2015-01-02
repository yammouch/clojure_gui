(ns gui00)

(import '[javafx.application Application])
;(import '[javafx.scene Scene])
;(import '[javafx.scene.control Label])
;(import '[javafx.scene.layout BorderPane])
(import '[javafx.stage Stage])

(gen-class
  :name gui00
  :main true
  :extends javafx.application.Application)

;(defn -init [this]
;  [[] (atom [])])

(defn -start [this ^Stage stage]
;  (let [label (Label. "This is JavaFX!")
;        pane (.setCenter (BorderPane.) label)
;        scene (Scene. pane 320 240)]
;    (.setScene stage scene)
;    (.show stage)))
  (.show stage))

(defn -main [& args]
  (Application/launch (Class/forName "gui00") (into-array String [])))


