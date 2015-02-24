(ns no_canvas_every_new)

(gen-class
  :name "no_canvas_every_new"
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

(defn make-square [[x y]]
  (let [square (Rectangle. x y 4.0 4.0)]
    (doto square
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    square))

(defn make-circle [[x y]]
  (let [circle (Circle. (+ x 2.0) (+ y 2.0) 2.0)]
    (doto circle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    circle))

(defn make-triangle [[x y]]
  (let [triangle (Polygon. (double-array
                  [x y (+ x 4.0) (+ y 2.0) x (+ y 4.0)]
                  ))]
    (doto triangle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    triangle))

(defn make-and [[x y]]
  (let [path (Path. (into-array PathElement
              [(MoveTo. x y)
               (LineTo. (+ x 2.0) y)
               (ArcTo. 2.0   ; radiusX
                       2.0   ; radiusY
                       0.0   ; AxisRotation
                       (+ x 2.0) ; x
                       (+ y 4.0) ; y
                       false ; largeArcFlag
                       true  ; sweepFlag (true -> counter clockwise)
                       )
               (LineTo. x (+ y 4.0))
               (ClosePath.)
               ]))]
    (doto path
      (.setStroke Color/TRANSPARENT)
      (.setFill Color/TRANSPARENT))
    path))

(defn make-objects [[xorg yorg]]
  (mapcat (fn [y]
            (map (fn [x fobj] (fobj [x y]))
                 (take 100 (iterate #(+ % 4.0) xorg))
                 (drop (mod y 4)
                       (cycle [make-square make-circle
                               make-triangle make-and]))))
          (take 100 (iterate #(+ % 4.0) yorg))))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(def key-commands
  {KeyCode/RIGHT #(alter origin assoc 0 (inc (@origin 0)))
   KeyCode/LEFT  #(alter origin assoc 0 (dec (@origin 0)))
   KeyCode/UP    #(alter origin assoc 1 (inc (@origin 1)))
   KeyCode/DOWN  #(alter origin assoc 1 (dec (@origin 1)))})

(defn pane-schem-key [f-set-to-parent pane]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [f (key-commands (.getCode keyEvent))]
        (when f
          (dosync (f))
          (.setAll (.getChildren pane)
                   (into-array Node (make-objects @origin)))
          (.consume keyEvent)
          )))))

(defn pane-schem [f-set-to-parent]
  (let [pane (Pane.)]
    (.setOnKeyPressed pane (pane-schem-key f-set-to-parent pane))
    (.setFocusTraversable pane true)
    (.setAll (.getChildren pane)
             (into-array Node (make-objects @origin)))
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
  (Application/launch (Class/forName "no_canvas_every_new")
                      (into-array String [])))

