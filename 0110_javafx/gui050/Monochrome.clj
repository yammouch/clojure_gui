(ns Monochrome)

(gen-class
  :name "Monochrome"
  :extends javafx.application.Application)

(import '[javafx.application Application])
(import '[javafx.scene Group Node Scene])
(import '[javafx.scene.paint Color])
(import '[javafx.scene.shape Circle StrokeType])
(import '[javafx.stage Stage])

(defn -main [& args]
  (Application/launch (into-array String args)))

(defn -start [self primary-stage]
  (let [root (Group.)
        scene (Scene. root 800.0 600.0 Color/BLACK)
        circle (Circle. 150 (Color/web "white" 0.0))]
    (.setStrokeType circle StrokeType/OUTSIDE)
    (.setStroke circle (Color/web "white" 1.0))
    (.setStrokeWidth circle 4)
    (.setScene primary-stage scene)
    (.. root getChildren (add circle))
    (.show primary-stage)
    ))
