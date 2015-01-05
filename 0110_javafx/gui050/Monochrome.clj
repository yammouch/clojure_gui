(ns Monochrome)

(gen-class
  :name "Monochrome"
  :extends javafx.application.Application)

(import '[javafx.animation KeyFrame KeyValue Timeline])
(import '[javafx.application Application])
(import '[javafx.scene Group Node Scene])
(import '[javafx.scene.effect BlendMode BoxBlur])
(import '[javafx.scene.paint Color CycleMethod LinearGradient Stop])
(import '[javafx.scene.shape Circle Rectangle StrokeType])
(import '[javafx.stage Stage])
(import '[javafx.util Duration])

(defn -main [& args]
  (Application/launch (into-array String args)))

(defn -start [self primary-stage]
  (let [root (Group.)
        scene (Scene. root 800.0 600.0 Color/BLACK)
        circles (Group.)
        colors ( Rectangle.
                 (.getWidth  scene)
                 (.getHeight scene)
                 ( LinearGradient.
                   0.0 1.0 1.0 0.0 true CycleMethod/NO_CYCLE
                   ( into-array Stop
                     [ (Stop. 0.00 (Color/web "#F8BD55"))
                       (Stop. 0.14 (Color/web "#C0FE56"))
                       (Stop. 0.28 (Color/web "#5DFBC1"))
                       (Stop. 0.43 (Color/web "#64C2F8"))
                       (Stop. 0.57 (Color/web "#BE4AF7"))
                       (Stop. 0.71 (Color/web "#ED5FC2"))
                       (Stop. 0.85 (Color/web "#EF504C"))
                       (Stop. 1.00 (Color/web "#F2660F"))
                       ])))
        timeline (Timeline.)]
    (.setScene primary-stage scene)
    (doseq [_ (range 30)]
      (let [circle (Circle. 150 (Color/web "white" 0.05))]
        (.setStrokeType circle StrokeType/OUTSIDE)
        (.setStroke circle (Color/web "white" 0.16))
        (.setStrokeWidth circle 4)
        (.. circles getChildren (add circle))))
    (.. colors widthProperty  (bind (.widthProperty  scene)))
    (.. colors heightProperty (bind (.heightProperty scene)))
    (.setBlendMode colors BlendMode/OVERLAY)
    (.. root getChildren
        (add ( Group.
               ( into-array Node
                 [ ( Group.
                     ( into-array Node
                       [ ( Rectangle.
                           (.getWidth scene) (.getHeight scene) Color/BLACK)
                         circles]))
                   colors]))))
    (.setEffect circles (BoxBlur. 10 10 3))
    (doseq [circle (.getChildren circles)]
      (.. timeline getKeyFrames
          ( addAll
            ( into-array KeyFrame
              [ ( KeyFrame.
                  Duration/ZERO
                  ( into-array KeyValue
                    [ (KeyValue. (.translateXProperty circle)
                                 (* (Math/random) 800))
                      (KeyValue. (.translateYProperty circle)
                                 (* (Math/random) 600)
                                 )]))
                ( KeyFrame.
                  (Duration. 40000)
                  ( into-array KeyValue
                    [ (KeyValue. (.translateXProperty circle)
                                 (* (Math/random) 800))
                      (KeyValue. (.translateYProperty circle)
                                 (* (Math/random) 600)
                                 )]))]))))
    (.play timeline)
    (.show primary-stage)
    ))
