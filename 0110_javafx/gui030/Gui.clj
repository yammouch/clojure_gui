(ns Gui)

(gen-class
  :name "Gui"
  :extends javafx.application.Application)

;(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

(import '[javafx.application Application])
(import '[javafx.scene Scene])
(import '[javafx.scene.control Button])
(import '[javafx.scene.layout VBox])
(import '[javafx.stage Stage])

(defn -start [self stage]
  (let [button (Button. "Say \"Hello Clojure\"!")
        vbox (VBox. (into-array [button]))
        scene (Scene. vbox 640 480)]
    (.setTitle stage "Hello JavaFX")
    (.setScene stage scene)
    (.show stage)
    ))

(defn -main [& args]
  (Application/launch (into-array String [])))

