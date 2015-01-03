; referred:
; https://coderwall.com/p/4yjy1a/getting-started-with-javafx-in-clojure

(ns Gui)

(import '[javafx.scene SceneBuilder])
(import '[javafx.scene.control ButtonBuilder])
(import '[javafx.scene.layout VBoxBuilder])
(import '[javafx.stage StageBuilder])

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

(def stage (atom nil))

(defn make-stage []
  (let [button (.. ButtonBuilder create
                   (text "Say \"Hello Clojure\"!")
                   build)
        vbox (.. VBoxBuilder create
                 (minHeight 480) (minWidth 640)
                 (children [button]) build)
        scene (.. SceneBuilder create
                  (height 480) (width 640)
                  (root vbox) build)
        stage-local (.. StageBuilder create
                        (title "Hello JavaFX")
                        (scene scene) build)]
    (reset! stage stage-local)
    ))

(defn -main []
  (javafx.application.Platform/runLater make-stage)
  (javafx.application.Platform/runLater #(.show @stage)))

