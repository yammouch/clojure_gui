(ns no_canvas_change)

(gen-class
  :name "no_canvas_change"
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
    square))

(defn make-circle [[x y]]
  (let [circle (Circle. (+ x (* 0.5 grid)) (+ y (* 0.5 grid)) (* 0.5 grid))]
    (doto circle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    circle))

(defn make-triangle [[x y]]
  (let [triangle (Polygon. (double-array
                  [x y (+ x grid) (+ y (* 0.5 grid)) x (+ y grid)]
                  ))]
    (doto triangle
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    triangle))

(defn make-and [[x y]]
  (let [path (Path. (into-array PathElement
              [(MoveTo. x y)
               (LineTo. (+ x (* 0.5 grid)) y)
               (ArcTo. (* 0.5 grid)       ; radiusX
                       (* 0.5 grid)       ; radiusY
                       0.0                ; AxisRotation
                       (+ x (* 0.5 grid)) ; x
                       (+ y grid) ; y
                       false ; largeArcFlag
                       true  ; sweepFlag (true -> counter clockwise)
                       )
               (LineTo. x (+ y grid))
               (ClosePath.)
               ]))]
    (doto path
      (.setStroke Color/BLACK)
      (.setFill Color/TRANSPARENT))
    path))

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
  (Application/launch (Class/forName "no_canvas_change")
                      (into-array String [])))

