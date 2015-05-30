(ns show_canvas_size)

(gen-class
  :name "show_canvas_size"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application   Application)
  '(javafx.event         EventHandler)
  '(javafx.geometry      Insets)
  '(javafx.scene         Group Node Scene)
  '(javafx.scene.canvas  Canvas GraphicsContext)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane Pane)
  '(javafx.scene.paint   Color)
  '(javafx.stage         Stage))

(def origin (ref [0 0]))
(def grid 8.0)

(defn draw-square [gc [x y]]
  (.strokeRect gc x y grid grid))

(defn draw-circle [gc [x y]]
  (.strokeOval gc x y grid grid))

(defn draw-triangle [gc [x y]]
  (.strokePolygon gc
   (double-array [x (+ x grid)         x])
   (double-array [y (+ y (* 0.5 grid)) (+ y grid)])
   3))

(defn draw-and [gc [x y]]
  (.beginPath gc)
  (.moveTo gc x y)
  (.lineTo gc (+ x (* 0.5 grid)) y)
  (.arcTo gc (+ x grid)
             (+ y (* 0.5 grid))
             (+ x (* 0.5 grid))
             (+ y grid)
             (* 0.5 grid))
  (.lineTo gc x (+ y grid))
  (.closePath gc)
  (.stroke gc))

(defn draw-objects [gc [xorg yorg]]
  (mapcat (fn [y-in-grid]
            (let [y (+ (* y-in-grid grid) yorg)]
              (map (fn [x fdraw] (fdraw gc [x y]))
                   (take 10 (iterate #(+ % grid) xorg))
                   (drop (mod y-in-grid 4)
                         (cycle [draw-square draw-circle
                                 draw-triangle draw-and])))))
          (range 10)))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(def key-commands
  {KeyCode/RIGHT (fn [] (alter origin update-in [0] #(+ % grid)))
   KeyCode/LEFT  (fn [] (alter origin update-in [0] #(- % grid)))
   KeyCode/UP    (fn [] (alter origin update-in [1] #(- % grid)))
   KeyCode/DOWN  (fn [] (alter origin update-in [1] #(+ % grid)))})

(defn pane-schem-key [f-set-to-parent gc canvas]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [f (key-commands (.getCode keyEvent))]
        (when f
          (dosync (f))
          (.clearRect gc 0.0 0.0 (.getWidth canvas) (.getHeight canvas))
          (dorun (draw-objects gc @origin))
          (.strokeRect gc 0 0 (.getWidth canvas) (.getHeight canvas))
          (.consume keyEvent)
          )))))

(defn pane-schem [f-set-to-parent]
  (let [canvas (Canvas. 400.0 300.0)
        gc (.getGraphicsContext2D canvas)]
    (.setStroke gc Color/BLACK)
    (.setFill gc Color/TRANSPARENT)
    (.setOnKeyPressed canvas
                      (pane-schem-key f-set-to-parent gc canvas))
    (.setFocusTraversable canvas true)
    (dorun (draw-objects gc @origin))
    (.strokeRect gc 0 0 (.getWidth canvas) (.getHeight canvas))
    (f-set-to-parent canvas)
    canvas))

;--------------------------------------------------
; JavaFX main routine
;--------------------------------------------------
(defn -start [self stage]
  (let [topgroup (BorderPane.)
        pane (pane-schem #(.setCenter topgroup %))]
    (doto stage
      (.setWidth 400) (.setHeight 300)
      (.setScene (Scene. topgroup))
      (.setTitle "Draws a lot of objects")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "show_canvas_size")
                      (into-array String [])))
