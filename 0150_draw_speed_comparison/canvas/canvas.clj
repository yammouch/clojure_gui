(ns canvas)

(gen-class
  :name "canvas"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application   Application)
  '(javafx.event         EventHandler)
  '(javafx.scene         Group Node Scene)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane Pane)
  '(javafx.scene.paint   Color)
  '(javafx.scene.shape   Rectangle Polygon Circle
                         Path PathElement MoveTo ArcTo ClosePath
                         LineTo)
  '(javafx.stage         Stage))

(def origin (ref [0 0]))
(def grid 8.0)

(defn make-square [[x y]]
  (let [square (Rectangle. x y grid grid)]
    (doto square
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    [square
     (fn [[x y]]
       (.setX square x)
       (.setY square y)
       )]))

(defn make-circle [[x y]]
  (let [circle (Circle. (+ x (* 0.5 grid)) (+ y (* 0.5 grid)) (* 0.5 grid))]
    (doto circle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    [circle
     (fn [[x y]]
       (.setCenterX circle (+ x (* 0.5 grid)))
       (.setCenterY circle (+ y (* 0.5 grid)))
       )]))

(defn make-triangle [[x y]]
  (let [triangle (Polygon. (double-array
                  [x y (+ x grid) (+ y (* 0.5 grid)) x (+ y grid)]
                  ))]
    (doto triangle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    [triangle
     (fn [[x y]]
     ;  (.. triangle getPoints
     ;      (setAll (double-array
     ;               [x y (+ x grid) (+ y (* 0.5 grid)) x (+ y grid)]
     ;               ))))]))
       )]))

(defn make-and [[x y]]
  (let [move-to (MoveTo. x y)
        line-to1 (LineTo. (+ x (* 0.5 grid)) y)
        arc-to (ArcTo. (* 0.5 grid)       ; radiusX
                       (* 0.5 grid)       ; radiusY
                       0.0                ; AxisRotation
                       (+ x (* 0.5 grid)) ; x
                       (+ y grid)         ; y
                       false              ; largeArcFlag
                       true ; sweepFlag (true -> counter clockwise)
                       )
        line-to2 (LineTo. x (+ y grid))
        path (Path. (into-array PathElement
              [move-to line-to1 arc-to line-to2 (ClosePath.)]
              ))]
    (doto path
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    [path
     (fn [[x y]]
       (doto move-to (.setX x) (.setY y))
       (doto line-to1 (.setX (+ x (* 0.5 grid))) (.setY y))
       (doto arc-to (.setX (+ x (* 0.5 grid))) (.setY (+ y grid)))
       (doto line-to2 (.setX x) (.setY (+ y grid)))
       )]))

(defn change-position [ll [xorg yorg]]
  (doseq [[f [x y]] (mapcat (fn [l y]
                              (map (fn [f x] [f [x y]])
                              l
                              (iterate #(+ % grid) xorg)))
                            (partition 100 ll)
                            (iterate #(+ % grid) yorg))]
    (f [x y])))

(defn make-objects [[xorg yorg]]
  (mapcat (fn [y-in-grid]
            (let [y (+ (* y-in-grid grid) yorg)]
              (map (fn [x fobj] (fobj [x y]))
                   (take 100 (iterate #(+ % grid) xorg))
                   (drop (mod y-in-grid 4)
                         (cycle [make-square make-circle
                                 make-triangle make-and])))))
          (range 100)))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(def key-commands
  {KeyCode/RIGHT (fn [] (alter origin update-in [0] #(+ grid %)))
   KeyCode/LEFT  (fn [] (alter origin update-in [0] #(- grid %)))
   KeyCode/UP    (fn [] (alter origin update-in [1] #(- grid %)))
   KeyCode/DOWN  (fn [] (alter origin update-in [1] #(+ grid %)))})

(defn pane-schem-key [f-set-to-parent ll]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [f (key-commands (.getCode keyEvent))]
        (when f
          (dosync (f))
          (change-position ll @origin)
          (.consume keyEvent)
          )))))

(defn pane-schem [f-set-to-parent]
  (let [objects (make-objects @origin)
        pane (Pane.)]
    (.setOnKeyPressed pane
                      (pane-schem-key f-set-to-parent
                                      (map #(% 1) objects)))
    (.setFocusTraversable pane true)
    (.setAll (.getChildren pane)
             (into-array Node (map #(% 0) objects)))
    (f-set-to-parent pane)
    pane))

;--------------------------------------------------
; JavaFX main routine
;--------------------------------------------------
(defn -start [self stage]
  (let [topgroup (BorderPane.)
        pane (pane-schem #(.setCenter topgroup %))]
    (doto stage
      (.setWidth 1024) (.setHeight 768)
      (.setScene (Scene. topgroup))
      (.setTitle "Draws a lot of objects")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "canvas")
                      (into-array String [])))
