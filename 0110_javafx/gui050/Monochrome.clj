(ns Monochrome)

(gen-class
  :name "Monochrome"
  :main true
  :extends javafx.application.Application)

(import '[javafx.application Application])
(import '[javafx.scene Group Node Scene])
(import '[javafx.scene.paint Color])
(import '[javafx.scene.shape Circle StrokeType])
(import '[javafx.stage Stage])

(def circle (atom []))

(defn -main [& args]
  (.start (Thread. (fn []
  (Application/launch (Class/forName "Monochrome")
                      (into-array String args))
  )))
    (read-line)
    (.setCenterX @circle 100.0)
    (read-line)
    (.setCenterX @circle 0.0)
)

(defn -start [self primary-stage]
  (let [root (Group.)
        scene (Scene. root 800.0 600.0 Color/BLACK)]
    (reset! circle (Circle. 150))
    (.setStrokeType @circle StrokeType/OUTSIDE)
    (.setStroke @circle (Color/web "white" 1.0))
    (.setStrokeWidth @circle 4)
    (.setScene primary-stage scene)
    (.. root getChildren (add @circle))
    (.show primary-stage)
    ))
