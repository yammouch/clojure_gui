(ns SchemRtl)

(gen-class
  :name "SchemRtl"
  :main true
  :extends javafx.application.Application)

(import
  '(java.io              File PushbackReader)
  '(javafx.application   Application)
  '(javafx.event         EventHandler)
  '(javafx.geometry      VPos)
  '(javafx.scene         Group Node Scene)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane Pane VBox FlowPane)
  '(javafx.scene.paint   Color)
  '(javafx.scene.shape   Rectangle Polygon Polyline Ellipse Line Circle
                         Path PathElement MoveTo ArcTo ClosePath
                         LineTo)
  '(javafx.scene.text    Font Text TextAlignment)
  '(javafx.scene.control Label TextField RadioButton ToggleGroup Button
                         MenuBar Menu MenuItem)
  '(javafx.stage         Stage FileChooser FileChooser$ExtensionFilter))
(require 'clojure.set)
(require 'clojure.java.io)
(require 'clojure.pprint)

(def pix-per-grid 8.0)
(defn grid2screen [[grid-x grid-y]]
  [(* pix-per-grid grid-x)
   (* pix-per-grid grid-y)])

;--------------------------------------------------
; state
;--------------------------------------------------

(def *label-debug* (Label.))

(def cursor-pos (ref {:x 5 :y 5}))
(def cursor-speed (ref 1))

(def selected-lels (ref #{}))
(def selected-wires (ref {}))
(def selected-name (ref nil))

(def mode (ref {:mode 'cursor}))
(def wire-p0 (ref {:x 0 :y 0}))
(def catalog-pos (ref {:x 0 :y 0}))

(def lels
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               '[{:type name , :x 5 , :y 20,
                  string "hoge", v-align bottom, h-align left}
                 {:type in   , :x 25, :y 28, direction right}
                 {:type in   , :x 25, :y 22, direction right}
                 {:type and  , :x 32, :y 28, direction right,
                  width 4, height 4}
                 {:type or   , :x 40, :y 23, direction right,
                  width 4, height 4}
                 {:type in   , :x 25, :y 30, direction right}
                 {:type in   , :x 25, :y 36, direction right}
                 {:type out  , :x 62, :y 26, direction right}
                 {:type in   , :x 25, :y 34, direction right}
                 {:type and  , :x 32, :y 22, direction right,
                  width 4, height 4}
                 {:type dot  , :x 30, :y 29}
                 {:type dff  , :x 55, :y 26}
                 {:type mux21, :x 48, :y 24, direction right,
                  width 2, height 6, order01 :1->0}
                 ])))

(def wires
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               [{:x0 36, :y0 24, :x1 41, :y1 24}
                {:x0 46, :y0 29, :x1 48, :y1 29}
                {:x0 28, :y0 29, :x1 32, :y1 29}
                {:x0 46, :y0 35, :x1 46, :y1 29}
                {:x0 30, :y0 29, :x1 30, :y1 25}
                {:x0 28, :y0 37, :x1 49, :y1 37}
                {:x0 36, :y0 30, :x1 38, :y1 30}
                {:x0 49, :y0 37, :x1 49, :y1 29}
                {:x0 50, :y0 27, :x1 55, :y1 27}
                {:x0 28, :y0 23, :x1 32, :y1 23}
                {:x0 28, :y0 31, :x1 32, :y1 31}
                {:x0 28, :y0 35, :x1 46, :y1 35}
                {:x0 44, :y0 25, :x1 48, :y1 25}
                {:x0 44, :y0 25, :x1 44, :y1 25}
                {:x0 38, :y0 30, :x1 38, :y1 26}
                {:x0 59, :y0 27, :x1 62, :y1 27}
                {:x0 30, :y0 25, :x1 32, :y1 25}
                {:x0 38, :y0 26, :x1 41, :y1 26}
                ])))

;--------------------------------------------------
; rotate
;--------------------------------------------------
(defn rotate [[x y] degree] ; clockwise, because y gets larger downward
  (case degree
    0   [   x     y ]
    90  [(- y)    x ]
    180 [(- x) (- y)]
    270 [   y  (- x)]))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-text [pos str color v-align h-align]
  (let [text (Text. str)]
    (doto text
      (.setFont (Font. "monospaced Regular" 12.0))
      (.setY (+ (* (:y pos) pix-per-grid)
                (case v-align bottom -2.0, center 0.0, top 2.0)))
      (.setTextOrigin (case v-align
                        bottom VPos/BOTTOM
                        center VPos/CENTER
                        top    VPos/TOP))
      (.setStroke color))
    (let [width (.. text getLayoutBounds getWidth)]
      (.setX text (- (* (:x pos) pix-per-grid)
                     (case h-align
                       left 1.0, center (* 0.5 width), right (- width 1.0)
                       ))))
    text))

(defn draw-dot [pos size color]
  (Circle. (* (:x pos) pix-per-grid)
           (* (:y pos) pix-per-grid)
           (* 0.5 size) color))

(defn draw-wire [{x0 :x0 y0 :y0 x1 :x1 y1 :y1} color]
  (let [line (Line. (* x0 pix-per-grid)
                    (* y0 pix-per-grid)
                    (* x1 pix-per-grid)
                    (* y1 pix-per-grid))]
    (.setStroke line color)
    line))

(defn draw-wire-selected [{x0 :x0 y0 :y0 x1 :x1 y1 :y1} selected]
  (if (= selected 'p0p1)
    (let [line (Line. (* pix-per-grid x0)
                      (* pix-per-grid y0)
                      (* pix-per-grid x1)
                      (* pix-per-grid y1))]
      (.setStroke line Color/RED)
      [line])
    (let [x- (- x1 x0) y- (- y1 y0)
          len (Math/sqrt (+ (* x- x-) (* y- y-)))

          [xorg xhl yorg yhl] ; hl: highlight
          (if (= selected 'p0)
            [x0 (+ x0 (/ x- len)) y0 (+ y0 (/ y- len))]
            [x1 (- x1 (/ x- len)) y1 (- y1 (/ y- len))])
          shortline (Line. (* pix-per-grid xorg)
                           (* pix-per-grid yorg)
                           (* pix-per-grid xhl)
                           (* pix-per-grid yhl))
          longline (Line. (* pix-per-grid xhl)
                          (* pix-per-grid yhl)
                          (* pix-per-grid (if (= selected 'p0) x1 x0))
                          (* pix-per-grid (if (= selected 'p0) y1 y0)))]
      (.setStroke shortline Color/RED)
      [shortline longline])))

(defn draw-status [objs]
  (map (fn [obj ypos]
         (let [text (Text. 2.0 (+ 12.0 (* 12.0 ypos))
                           (if (nil? obj) "nil" (.toString obj)))]
           (doto text
             (.setFont (Font. "Monospaced Regular" 10.0))
             (.setTextAlignment TextAlignment/LEFT)
             (.setTextOrigin VPos/TOP)
             (.setStroke Color/BLUE))
           text))
       objs (range)))

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------

(defn lel-init [type]
  (case type
    in    {:type 'in    :x 0 :y 0 'direction 'right}
    out   {:type 'out   :x 0 :y 0 'direction 'right}
    inout {:type 'inout :x 0 :y 0 'direction 'horizontal}
    dot   {:type 'dot   :x 0 :y 0}
    name  {:type 'name  :x 0 :y 0
           'string "blah" 'v-align 'bottom 'h-align 'left}
    not   {:type 'not   :x 0 :y 0 'direction 'right}
    and   {:type 'and   :x 0 :y 0 'direction 'right 'width 4 'height 4}
    or    {:type 'or    :x 0 :y 0 'direction 'right 'width 4 'height 4}
    dff   {:type 'dff   :x 0 :y 0}
    dffr  {:type 'dffr  :x 0 :y 0}
    mux21 {:type 'mux21 :x 0 :y 0 'direction 'right 'width 2 'height 6
           'order01 :0->1}
    mux-n {:type 'mux-n :x 0 :y 0 'direction 'right 'width 2 'height 6}
    plus  {:type 'plus  :x 0 :y 0}
    minus {:type 'minus :x 0 :y 0}
    ))

(defmulti lel-width  (fn [lel] (:type lel)))
(defmulti lel-height (fn [lel] (:type lel)))
(defmulti lel-x-min  (fn [lel] (:type lel)))
(defmulti lel-x-max  (fn [lel] (:type lel)))
(defmulti lel-y-min  (fn [lel] (:type lel)))
(defmulti lel-y-max  (fn [lel] (:type lel)))
(defmulti lel-draw   (fn [lel color & xs] (:type lel)))


; Following declarations look redundant at this commit.
; But width and height will be variables after adding some size change
; features.

; for "in"
(defmethod lel-width  'in [lel]
  (case (lel 'direction) (right left) 3, (up down) 2))
(defmethod lel-height 'in [lel]
  (case (lel 'direction) (right left) 2, (up down) 3))
(defmethod lel-x-min  'in [lel] (:x lel))
(defmethod lel-x-max  'in [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'in [lel] (:y lel))
(defmethod lel-y-max  'in [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'in [lel color]
  (let [symbol (Polygon. (double-array (apply concat
                (map #(list (* pix-per-grid (+ (:x lel) (% 0)))
                            (* pix-per-grid (+ (:y lel) (% 1))))
                     (case (lel 'direction)
                       right [[0 0] [2 0] [3 1] [2 2] [0 2]]
                       up    [[0 3] [0 1] [1 0] [2 1] [2 3]]
                       left  [[3 0] [1 0] [0 1] [1 2] [3 2]]
                       down  [[0 0] [0 2] [1 3] [2 2] [2 0]])))))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (:x lel) (* 0.5 (lel-width  lel)))
                 :y (+ (:y lel) (* 0.5 (lel-height lel)))}
                "I" color 'center 'center)
     symbol]))

; for "out"
(defmethod lel-width  'out [lel]
  (case (lel 'direction) (right left) 3, (up down) 2))
(defmethod lel-height 'out [lel]
  (case (lel 'direction) (right left) 2, (up down) 3))
(defmethod lel-height 'out [lel] 2)
(defmethod lel-x-min  'out [lel] (:x lel))
(defmethod lel-x-max  'out [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'out [lel] (:y lel))
(defmethod lel-y-max  'out [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'out [lel color]
  (let [symbol (Polygon. (double-array (apply concat
                (map #(list (* pix-per-grid (+ (:x lel) (% 0)))
                            (* pix-per-grid (+ (:y lel) (% 1))))
                     (case (lel 'direction)
                       right [[0 0] [2 0] [3 1] [2 2] [0 2]]
                       up    [[0 3] [0 1] [1 0] [2 1] [2 3]]
                       left  [[3 0] [1 0] [0 1] [1 2] [3 2]]
                       down  [[0 0] [0 2] [1 3] [2 2] [2 0]])))))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (:x lel) (* 0.5 (lel-width  lel)))
                 :y (+ (:y lel) (* 0.5 (lel-height lel)))}
                "O" color 'center 'center)
     symbol]))

; for "inout"
(defmethod lel-width  'inout [lel]
  (case (lel 'direction) horizontal 3, vertical 2))
(defmethod lel-height 'inout [lel]
  (case (lel 'direction) horizontal 2, vertical 3))
(defmethod lel-x-min  'inout [lel] (:x lel))
(defmethod lel-x-max  'inout [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'inout [lel] (:y lel))
(defmethod lel-y-max  'inout [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'inout [lel color]
  (let [symbol
          (Polygon. (double-array (apply concat
           (map #(list (* pix-per-grid (+ (:x lel) (% 0)))
                       (* pix-per-grid (+ (:y lel) (% 1))))
                (case (lel 'direction)
                  horizontal [[0 1] [1 0] [2 0] [3 1] [2 2] [1 2]]
                  vertical   [[1 0] [0 1] [0 2] [1 3] [2 2] [2 1]]
                  )))))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (:x lel) (* 0.5 (lel-width  lel)))
                 :y (+ (:y lel) (* 0.5 (lel-height lel)))}
                "IO" color 'center 'center)
     symbol]))

; for "dot"
(defmethod lel-width  'dot [lel] 0)
(defmethod lel-height 'dot [lel] 0)
(defmethod lel-x-min  'dot [lel] (:x lel))
(defmethod lel-x-max  'dot [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'dot [lel] (:y lel))
(defmethod lel-y-max  'dot [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'dot [lel color]
  [(draw-dot lel 7 color)])

; for "name"
; 0 of size is feasible?
(defmethod lel-width  'name [lel] 0)
(defmethod lel-height 'name [lel] 0)
(defmethod lel-x-min  'name [lel] (:x lel))
(defmethod lel-x-max  'name [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'name [lel] (:y lel))
(defmethod lel-y-max  'name [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'name [lel color]
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)
        line-h (Line. (- x 1.0) y (+ x 1.0) y)
        line-v (Line. x (- y 1.0) x (+ y 1.0))]
    (.setStroke line-h color) (.setStroke line-v color)
    [(draw-text lel (lel 'string) color
                (lel 'v-align) (lel 'h-align))
     line-h line-v]))

; for "not"
(defmethod lel-width  'not [lel]
  (case (lel 'direction) (right left) 3, (up down) 4))
(defmethod lel-height 'not [lel]
  (case (lel 'direction) (right left) 4, (up down) 3))
(defmethod lel-x-min  'not [lel] (:x lel))
(defmethod lel-x-max  'not [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'not [lel] (:y lel))
(defmethod lel-y-max  'not [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'not [lel color]
  (let [triangle
          (Polygon. (double-array (apply concat
           (map #(list (* pix-per-grid (+ (lel :x) (% 0)))
                       (* pix-per-grid (+ (lel :y) (% 1))))
                (case (lel 'direction)
                  right [[0 0] [2 2] [0 4]]
                  up    [[2 1] [0 3] [4 3]]
                  left  [[1 2] [3 0] [3 4]]
                  down  [[0 0] [2 2] [4 0]]
                  )))))
        circle
          (Circle. (* pix-per-grid
                      (+ (lel :x)
                         (case (lel 'direction)
                           right 2.5, up 2.0, left 0.5, down 2.0)))
                   (* pix-per-grid
                      (+ (lel :y)
                         (case (lel 'direction)
                           right 2.0, up 0.5, left 2.0, down 2.5)))
                   (* 0.5 pix-per-grid))]
    (doto triangle (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto circle   (.setStroke color) (.setFill Color/TRANSPARENT))
    [triangle circle]))

; for "and"
; "and" should be extended according to needed inputs.
(defmethod lel-width  'and [lel] (lel 'width))
(defmethod lel-height 'and [lel] (lel 'height))
(defmethod lel-x-min  'and [lel] (:x lel))
(defmethod lel-x-max  'and [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'and [lel] (:y lel))
(defmethod lel-y-max  'and [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'and [lel color]
  (let [xorg (* (:x lel) pix-per-grid)
        yorg (* (:y lel) pix-per-grid)
        [angle xofs yofs] (case (lel 'direction)
                            right [  0 0 0], up   [270 0 1],
                            left  [180 1 1], down [ 90 1 0])
        [[x0 y0] [x1 y1] [x2 y2] [x3 y3]]
          (map #(let [[x y] (rotate % angle)]
                  [(+ xorg (* (+ x xofs) (lel 'width ) pix-per-grid))
                   (+ yorg (* (+ y yofs) (lel 'height) pix-per-grid))])
               [[0.0 0.0] [0.5 0.0] [0.5 1.0] [0.0 1.0]])
        rx (* 0.5 pix-per-grid (lel 'width))
        ry (* 0.5 pix-per-grid (lel 'height))
        symbol (Path. (into-array PathElement
                [(MoveTo. x0 y0)
                 (LineTo. x1 y1)
                 (ArcTo. rx    ; radiusX
                         ry    ; radiusY
                         0.0   ; AxisRotation
                         x2    ; x
                         y2    ; y
                         false ; largeArcFlag
                         true  ; sweepFlag (true -> counter clockwise)
                         )
                 (LineTo. x3 y3)
                 (ClosePath.)
                 ]))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [symbol]))

; for "or"
; "or" should be extended according to needed inputs.
(defmethod lel-width  'or [lel] (lel 'width))
(defmethod lel-height 'or [lel] (lel 'height))
(defmethod lel-x-min  'or [lel] (:x lel))
(defmethod lel-x-max  'or [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'or [lel] (:y lel))
(defmethod lel-y-max  'or [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'or [lel color]
  (let [xorg (* (:x lel) pix-per-grid)
        yorg (* (:y lel) pix-per-grid)
        [angle xofs yofs] (case (lel 'direction)
                            right [  0 0 0], up   [270 0 1],
                            left  [180 1 1], down [ 90 1 0])
        [[x0 y0] [x1 y1]]
          (map #(let [[x y] (rotate % angle)]
                  [(+ xorg (* pix-per-grid (lel 'width ) (+ x xofs)))
                   (+ yorg (* pix-per-grid (lel 'height) (+ y yofs)))])
               [[0.0 0.0] [0.0 1.0]])
        [rx0 ry0 rx1 ry1]
          (let [w (* (lel 'width ) pix-per-grid)
                h (* (lel 'height) pix-per-grid)]
            (if ('#{right left} (lel 'direction))
              [w         (* 0.5 h) (* 0.25 w) (* 0.5  h)]
              [(* 0.5 w) h         (* 0.5  w) (* 0.25 h)]))
        symbol (Path. (into-array PathElement
                [(MoveTo. x0 y0)
                 (ArcTo. rx0 ry0 0.0 x1 y1 false true)
                 (ArcTo. rx1 ry1 0.0 x0 y0 false false)
                 (ClosePath.)
                 ]))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [symbol]))

; for "dff"
(defmethod lel-width  'dff [lel] 4)
(defmethod lel-height 'dff [lel] 5)
(defmethod lel-x-min  'dff [lel] (:x lel))
(defmethod lel-x-max  'dff [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'dff [lel] (:y lel))
(defmethod lel-y-max  'dff [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'dff [lel color]
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)
        rect (Rectangle. x y (* 4 pix-per-grid) (* 5 pix-per-grid))
        line (Polyline. (double-array (apply concat
              (map #(list (* pix-per-grid (+ (:x lel) %1))
                          (* pix-per-grid (+ (:y lel) %2)))
                   [1 2 3]
                   [5 4 5]
                   ))))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto line (.setStroke color))
    [rect line]))

; for "dffr"
(defmethod lel-width  'dffr [lel] 4)
(defmethod lel-height 'dffr [lel] 5)
(defmethod lel-x-min  'dffr [lel] (:x lel))
(defmethod lel-x-max  'dffr [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'dffr [lel] (:y lel))
(defmethod lel-y-max  'dffr [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'dffr [lel color]
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)
        rect (Rectangle. x y (* 4 pix-per-grid) (* 5 pix-per-grid))
        line (Polyline. (double-array (apply concat
              (map #(list (* pix-per-grid (+ (:x lel) %1))
                          (* pix-per-grid (+ (:y lel) %2)))
                   [1 2 3]
                   [5 4 5]
                   ))))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto line (.setStroke color))
    [rect line
     (draw-text {:x (+ (:x lel) 2) :y (:y lel)}
                "R" color 'top 'center
                )]))

; for "mux21"
(defmethod lel-width  'mux21 [lel]
  (if ('#{up down} (lel 'direction))
    (lel 'width) (lel 'height)))
(defmethod lel-height 'mux21 [lel] 
  (if ('#{up down} (lel 'direction))
    (lel 'height) (lel 'width)))
(defmethod lel-x-min  'mux21 [lel] (:x lel))
(defmethod lel-x-max  'mux21 [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'mux21 [lel] (:y lel))
(defmethod lel-y-max  'mux21 [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'mux21 [lel color]
  (concat
   (lel-draw (assoc lel :type 'mux-n) color)
   (let [w (lel 'width) h (lel 'height)
         [angle xofs yofs] (case (lel 'direction)
                             right [  0 0 0], up   [270 0 w],
                             left  [180 w h], down [ 90 h 0])
         [[ax ay] [bx by]] (map #(map + (rotate % angle)
                                        [xofs yofs]
                                        [(:x lel) (:y lel)])
                                [[(* 0.5 w) 2] [(* 0.5 w) (- h 2)]]
                                )]
     [(draw-text {:x ax :y ay} (if (= (lel 'order01) :0->1) "0" "1")
                 color 'center 'center)
      (draw-text {:x bx :y by} (if (= (lel 'order01) :0->1) "1" "0")
                 color 'center 'center
                 )])))

; for "mux-n"
(defmethod lel-width  'mux-n [lel]
  (if ('#{up down} (lel 'direction))
    (lel 'width) (lel 'height)))
(defmethod lel-height 'mux-n [lel]
  (if ('#{up down} (lel 'direction))
    (lel 'height) (lel 'width)))
(defmethod lel-x-min  'mux-n [lel] (:x lel))
(defmethod lel-x-max  'mux-n [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'mux-n [lel] (:y lel))
(defmethod lel-y-max  'mux-n [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'mux-n [lel color]
  (let [h (lel 'height) w (lel 'width)
        [angle xofs yofs] (case (lel 'direction)
                            right [  0 0 0], up   [270 0 w],
                            left  [180 w h], down [ 90 h 0])
        trapezoid (Polygon. (double-array (apply concat
                   (map #(grid2screen (map + (rotate % angle)
                                             [xofs yofs]
                                             [(:x lel) (:y lel)]))
                        [[0 0] [w 2] [w (- h 2)] [0 h]]
                        ))))]
    (doto trapezoid (.setStroke color) (.setFill Color/TRANSPARENT))
    [trapezoid]))

; for "plus"
(defmethod lel-width  'plus [lel] 4)
(defmethod lel-height 'plus [lel] 4)
(defmethod lel-x-min  'plus [lel] (:x lel))
(defmethod lel-x-max  'plus [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'plus [lel] (:y lel))
(defmethod lel-y-max  'plus [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'plus [lel color]
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)
        rect ( Rectangle. x y (* 4 pix-per-grid) (* 4 pix-per-grid))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (lel :x) (* 0.5 (lel-width  lel)))
                 :y (+ (lel :y) (* 0.5 (lel-height lel)))}
                "+" color 'center 'center)
     rect]))

; for "minus"
(defmethod lel-width  'minus [lel] 4)
(defmethod lel-height 'minus [lel] 4)
(defmethod lel-x-min  'minus [lel] (:x lel))
(defmethod lel-x-max  'minus [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'minus [lel] (:y lel))
(defmethod lel-y-max  'minus [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'minus [lel color]
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)
        rect ( Rectangle. x y (* 4 pix-per-grid) (* 4 pix-per-grid))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (lel :x) (* 0.5 (lel-width  lel)))
                 :y (+ (lel :y) (* 0.5 (lel-height lel)))}
                "-" color 'center 'center)
     rect]))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor [cursor-pos lels wires]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (apply concat
           (map (fn [[k v]]
                  (let [selected (@selected-wires k)]
                    (if selected
                      (draw-wire-selected v selected)
                      [(draw-wire v Color/BLACK)])))
                wires))
    (apply concat
           (map (fn [[k v]]
                  (lel-draw v (if (@selected-lels k)
                                Color/RED
                                Color/BLACK)))
                lels))
    (when (@mode :rect-x0)
      (let [x (Math/min (cursor-pos :x) (@mode :rect-x0))
            y (Math/min (cursor-pos :y) (@mode :rect-y0))
            width  (Math/abs (- (cursor-pos :x) (@mode :rect-x0)))
            height (Math/abs (- (cursor-pos :y) (@mode :rect-y0)))
            rect (Rectangle. (* x pix-per-grid)
                             (* y pix-per-grid)
                             (* width pix-per-grid)
                             (* height pix-per-grid))]
        (.setFill rect Color/TRANSPARENT)
        (.setStroke rect Color/BLACK)
        (.setAll (.getStrokeDashArray rect)
                 ( into-array Double [2.0 2.0] ))
        [rect])))))

(defn draw-mode-add []
  (into-array Node
   (concat
    (map (fn [[k v]] (draw-wire v Color/BLACK))
         @wires)
    (apply concat
           (map (fn [[k v]] (lel-draw v Color/BLACK))
                @lels))
    (lel-draw (conj (lel-init (:type @mode))
                    @cursor-pos)
              Color/RED))))

(defn draw-mode-wire []
  (into-array Node
   (concat
    [(draw-dot @cursor-pos 9 Color/BLUE)]
    (map (fn [[k v]] (draw-wire v Color/BLACK))
         @wires)
    (apply concat
           (map (fn [[k v]] (lel-draw v Color/BLACK))
                @lels))
    [(draw-wire {:x0 (@wire-p0 :x) :y0 (@wire-p0 :y)
                 :x1 (@cursor-pos :x) :y1 (@cursor-pos :y)}
                Color/RED)])))

(def catalog-table
  '[[in    out   inout dot   name]
    [not   and   or    dff   dffr]
    [mux21 mux-n plus  minus     ]])

(defn draw-mode-catalog []
  (let [parts (apply concat
               (map (fn [idx0 parts]
                      (map (fn [idx1 part]
                             {:idx0 idx0, :idx1 idx1, :part part})
                           (range) parts))
                    (range) catalog-table))
        rect (Rectangle. (* pix-per-grid (+ (* 10 (@catalog-pos :x)) 1))
                         (* pix-per-grid (+ (* 10 (@catalog-pos :y)) 1))
                         (* pix-per-grid 10)
                         (* pix-per-grid 10))]
    (.setStroke rect Color/RED)
    (.setStrokeWidth rect 2.0)
    (.setFill rect Color/TRANSPARENT)
    (into-array Node
     (concat
      (apply concat
             (map (fn [{idx0 :idx0 idx1 :idx1 part :part}]
                    (let [lel (lel-init part)]
                      (lel-draw (conj lel
                                      {:x (- (+ (* 10 idx1) 6)
                                             (int (/ (lel-width lel) 2)))
                                       :y (- (+ (* 10 idx0) 6)
                                             (int (/ (lel-height lel) 2))
                                             )})
                                Color/BLACK)))
                  parts))
      [rect]))))

(defn draw-mode []
  (case (@mode :mode)
    cursor  (draw-mode-cursor @cursor-pos
                              @lels @wires)
    move    (draw-mode-cursor @cursor-pos
                              @lels @wires)
    add     (draw-mode-add)
    wire    (draw-mode-wire)
    catalog (draw-mode-catalog)
    ))

;--------------------------------------------------
; move-*
;--------------------------------------------------

(defn move-cursor [dir speed]
  (dosync
    (ref-set cursor-pos
             (case dir
               left  (assoc @cursor-pos :x (- (@cursor-pos :x) speed))
               right (assoc @cursor-pos :x (+ (@cursor-pos :x) speed))
               up    (assoc @cursor-pos :y (- (@cursor-pos :y) speed))
               down  (assoc @cursor-pos :y (+ (@cursor-pos :y) speed))
               ))))

(defn move-selected-lels [dir speed]
  (dosync
    (ref-set lels
             (reduce (fn [lels sel]
                       (assoc lels sel
                              (assoc (lels sel)
                                     (cond (#{'left 'right} dir) :x
                                           (#{'up 'down} dir)    :y)
                                     (case dir
                                       left  (- ((lels sel) :x) speed)
                                       right (+ ((lels sel) :x) speed)
                                       up    (- ((lels sel) :y) speed)
                                       down  (+ ((lels sel) :y) speed)
                                       ))))
                     @lels
                     @selected-lels))))

(defn move-wire [wire dir speed points]
  (let [[f & keys] (case [points dir]
                     [p0   left ] [#(- % speed) :x0]
                     [p0   right] [#(+ % speed) :x0]
                     [p0   up   ] [#(- % speed) :y0]
                     [p0   down ] [#(+ % speed) :y0]
                     [p1   left ] [#(- % speed) :x1]
                     [p1   right] [#(+ % speed) :x1]
                     [p1   up   ] [#(- % speed) :y1]
                     [p1   down ] [#(+ % speed) :y1]
                     [p0p1 left ] [#(- % speed) :x0 :x1]
                     [p0p1 right] [#(+ % speed) :x0 :x1]
                     [p0p1 up   ] [#(- % speed) :y0 :y1]
                     [p0p1 down ] [#(+ % speed) :y0 :y1])]
    (reduce (fn [wire k] (assoc wire k (f (wire k))))
            wire keys)))

(defn move-selected-wires [dir speed]
  (let [moved (reduce (fn [wires [sel points]]
                        (assoc wires sel
                               (move-wire (wires sel) dir speed points)))
                      @wires
                      @selected-wires)]
    (dosync
      (ref-set wires moved)
      )))

(defn move-selected [dir speed]
  (move-selected-lels dir speed)
  (move-selected-wires dir speed))

(defn move-catalog [dir]
  (dosync
    (ref-set catalog-pos
             (case dir
               left  (assoc @catalog-pos :x (dec (@catalog-pos :x)))
               right (assoc @catalog-pos :x (inc (@catalog-pos :x)))
               up    (assoc @catalog-pos :y (dec (@catalog-pos :y)))
               down  (assoc @catalog-pos :y (inc (@catalog-pos :y)))
               ))))

;--------------------------------------------------
; sub functions for key commands
;--------------------------------------------------

(defn release-selection []
  (dosync
    (ref-set selected-lels #{})
    (ref-set selected-wires {})))

(defn find-lel-by-pos [lels pos]
  (some (fn [[k v]]
          (when (and (= (pos :x) (v :x))
                     (= (pos :y) (v :y)))
            k))
        lels))

(defn wire-vs-cursor [wire cur]
  (let [fcomp (fn [qc q0 q1]
                (let [[q0 q1 inv] (if (< q0 q1)
                                    [q0 q1 false]
                                    [q1 q0 true ])]
                  (cond (< qc q0) nil
                        (< q1 qc) nil

                        (< (- q1 q0) 4)
                        (cond (= qc q0) (if inv 'p1 'p0)
                              (= qc q1) (if inv 'p0 'p1)
                              :else     'p0p1)

                        (<= qc (+ q0 1)) (if inv 'p1 'p0)
                        (<= (- q1 1) qc) (if inv 'p0 'p1)
                        :else 'p0p1
                        )))]
    (cond (and (= (:x cur) (:x0 wire))
               (= (:y cur) (:y0 wire))) 'p0
          (and (= (:x cur) (:x1 wire))
               (= (:y cur) (:y1 wire))) 'p1

          (= (:x cur) (:x0 wire) (:x1 wire))
          (fcomp (:y cur) (:y0 wire) (:y1 wire))

          (= (:y cur) (:y0 wire) (:y1 wire))
          (fcomp (:x cur) (:x0 wire) (:x1 wire))

          :else nil)))

(defn find-wires-by-pos [wires pos]
  (let [rec (fn [ws acc]
              (if (empty? ws)
                acc
                (let [[k v] (first ws)
                      p (wire-vs-cursor v pos)]
                  (recur (rest ws)
                         (if p (conj acc {k p}) acc)
                         ))))]
    (rec wires {})))

(defn merge-selected-wire [base add]
  (letfn [(rec [xs acc]
            (if (empty? xs)
              acc
              (let [picked (base (ffirst xs))]
                (if picked
                  (recur (rest xs)
                         (conj acc
                               {(ffirst xs)
                                (case [picked (second (first xs))]
                                  [p0   p0  ] 'p0
                                  [p0   p1  ] 'p0p1
                                  [p0   p0p1] 'p0p1
                                  [p1   p0  ] 'p0p1
                                  [p1   p1  ] 'p1
                                  [p1   p0p1] 'p0p1
                                  [p0p1 p0  ] 'p0p1
                                  [p0p1 p1  ] 'p0p1
                                  [p0p1 p0p1] 'p0p1
                                  )}))
                  (recur (rest xs)
                         (conj acc (first xs))
                         )))))]
    (rec add base)))

(defn rectangular-select [lels wires x0 y0 x1 y1]
  (let [xmin (Math/min x0 x1) xmax (Math/max x0 x1)
        ymin (Math/min y0 y1) ymax (Math/max y0 y1)
        lels (filter (fn [[k v]] (<= xmin (lel-x-min v))) lels)
        lels (filter (fn [[k v]] (<= ymin (lel-y-min v))) lels)
        lels (filter (fn [[k v]] (<= (lel-x-max v) xmax)) lels)
        lels (filter (fn [[k v]] (<= (lel-y-max v) ymax)) lels)
        wires (filter (fn [[k v]]
                        (<= xmin (Math/min (:x0 v) (:x1 v))))
                      wires)
        wires (filter (fn [[k v]]
                        (<= ymin (Math/min (:y0 v) (:y1 v))))
                      wires)
        wires (filter (fn [[k v]]
                        (<= (Math/max (:x0 v) (:x1 v)) xmax))
                      wires)
        wires (filter (fn [[k v]]
                        (<= (Math/max (:y0 v) (:y1 v)) ymax))
                      wires)]
    {:lels (set (keys lels))
     :wires (zipmap (keys wires) (repeat 'p0p1))
     }))

(defn remove-lel-by-key [lels keys]
  (apply hash-map
         (apply concat
                (remove (fn [[k v]] (keys k))
                        lels))))

(defn remove-wire-by-key [wires keys]
  (apply hash-map
         (apply concat
                (remove (fn [[k v]]
                          (let [points (keys k)]
                            (= points 'p0p1)))
                        wires))))

;;(defn close-window [frame]
;;  (let [yn (JOptionPane/showConfirmDialog
;;            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
;;    (when (= yn JOptionPane/YES_OPTION)
;;      (.dispose frame))))
;;
;--------------------------------------------------
; key commands for each mode on schematic panel
;--------------------------------------------------

(def key-command-cursor-mode
  {KeyCode/LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/I      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< @cursor-speed 64)
                                (* 2 @cursor-speed)
                                64))))
   KeyCode/U      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< 1 @cursor-speed)
                                (/ @cursor-speed 2)
                                1))))
   ;KeyCode/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyCode/C      (fn [_] (dosync (ref-set mode {:mode 'catalog})))
   KeyCode/M      (fn [_] (dosync
                            (ref-set mode {:mode 'move})))
   KeyCode/W      (fn [_] (dosync
                            (release-selection)
                            (ref-set wire-p0 @cursor-pos)
                            (ref-set mode {:mode 'wire})))
   KeyCode/R
   (fn [_]
     (if (:rect-x0 @mode)
       (dosync (alter mode dissoc :rect-x0 :rect-y0))
       (dosync (alter mode conj {:rect-x0 (@cursor-pos :x)
                                 :rect-y0 (@cursor-pos :y)}))))
   KeyCode/ENTER
   (fn [_]
     (let [lel-key (find-lel-by-pos @lels @cursor-pos)
           wire-key (find-wires-by-pos @wires @cursor-pos)
           rect-keys (if (@mode :rect-x0)
                       (rectangular-select @lels @wires
                         (@mode :rect-x0) (@mode :rect-y0)
                         (@cursor-pos :x) (@cursor-pos :y))
                       {})]
       (dosync
         (when lel-key
           (alter selected-lels conj lel-key))
         (when (:lels rect-keys)
           (alter selected-lels clojure.set/union (:lels rect-keys)))
         (when wire-key
           (alter selected-wires merge-selected-wire wire-key))
         (when (:wires rect-keys)
           (alter selected-wires merge-selected-wire (:wires rect-keys)))
         (alter mode dissoc :rect-x0 :rect-y0)
         )))
   KeyCode/ESCAPE (fn [_]
                    (dosync (alter mode dissoc :rect-x0 :rect-y0))
                    (release-selection))
   KeyCode/X      (fn [_] (dosync
                            (alter lels remove-lel-by-key @selected-lels)
                            (alter wires remove-wire-by-key @selected-wires)
                            (ref-set selected-lels #{})
                            ))})

; add mode can be merged into move mode
; if continuous addition is not necessary.
(def key-command-add-mode
  {KeyCode/LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/I      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< @cursor-speed 64)
                                    (* 2 @cursor-speed)
                                    64))))
   KeyCode/U      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< 1 @cursor-speed)
                                    (/ @cursor-speed 2)
                                    1))))
   ;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   KeyCode/C      (fn [_] (dosync (ref-set mode {:mode 'catalog})))
   KeyCode/ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym)
               (conj (lel-init (:type @mode))
                     @cursor-pos)})))
   KeyCode/ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   })

(def key-command-move-mode
  {KeyCode/LEFT   (fn [_] (move-cursor   'left  @cursor-speed)
                          (move-selected 'left  @cursor-speed))
   KeyCode/RIGHT  (fn [_] (move-cursor   'right @cursor-speed)
                          (move-selected 'right @cursor-speed))
   KeyCode/UP     (fn [_] (move-cursor   'up    @cursor-speed)
                          (move-selected 'up    @cursor-speed))
   KeyCode/DOWN   (fn [_] (move-cursor   'down  @cursor-speed)
                          (move-selected 'down  @cursor-speed))
   KeyCode/H      (fn [_] (move-cursor   'left  @cursor-speed)
                          (move-selected 'left  @cursor-speed))
   KeyCode/L      (fn [_] (move-cursor   'right @cursor-speed)
                          (move-selected 'right @cursor-speed))
   KeyCode/K      (fn [_] (move-cursor   'up    @cursor-speed)
                          (move-selected 'up    @cursor-speed))
   KeyCode/J      (fn [_] (move-cursor   'down  @cursor-speed)
                          (move-selected 'down  @cursor-speed))
   KeyCode/I      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< @cursor-speed 64)
                                (* 2 @cursor-speed)
                                64))))
   KeyCode/U      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< 1 @cursor-speed)
                                (/ @cursor-speed 2)
                                1))))
   ;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   KeyCode/ESCAPE (fn [_] (dosync
                            (release-selection)
                            (ref-set mode {:mode 'cursor})))
                            })

(def key-command-wire-mode
  {KeyCode/LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyCode/L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyCode/K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyCode/J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyCode/I      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< @cursor-speed 64)
                                (* 2 @cursor-speed)
                                64))))
   KeyCode/U      (fn [_] (dosync
                            (ref-set cursor-speed
                              (if (< 1 @cursor-speed)
                                (/ @cursor-speed 2)
                                1))))
   ;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   KeyCode/ESCAPE (fn [_] (dosync
                            (ref-set mode {:mode 'cursor})))
   KeyCode/ENTER  (fn [_] (dosync
                            (alter wires conj
                                  {(gensym) {:x0 (@wire-p0 :x)
                                             :y0 (@wire-p0 :y)
                                             :x1 (@cursor-pos :x)
                                             :y1 (@cursor-pos :y)}})
                            (ref-set mode {:mode 'cursor})))
                            })

(def key-command-catalog-mode
  {KeyCode/LEFT   (fn [_] (move-catalog 'left))
   KeyCode/RIGHT  (fn [_] (move-catalog 'right))
   KeyCode/UP     (fn [_] (move-catalog 'up))
   KeyCode/DOWN   (fn [_] (move-catalog 'down))
   KeyCode/H      (fn [_] (move-catalog 'left))
   KeyCode/L      (fn [_] (move-catalog 'right))
   KeyCode/K      (fn [_] (move-catalog 'up))
   KeyCode/J      (fn [_] (move-catalog 'down))
   ;KeyCode/Q      (fn [{frame :frame}] (close-window frame))

   KeyCode/ENTER
   (fn [_]
     (let [type (try
                  (nth (nth catalog-table
                            (:y @catalog-pos))
                       (:x @catalog-pos))
                  (catch IndexOutOfBoundsException e nil))]
       (dosync
         (ref-set mode
                  (if type
                    {:mode 'add :type type}
                    {:mode 'cursor})))))

   KeyCode/ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   })

(def key-command
  {'cursor  key-command-cursor-mode
   'add     key-command-add-mode
   'move    key-command-move-mode
   'wire    key-command-wire-mode
   'catalog key-command-catalog-mode
   })

(defn state-text []
  (reduce #(str %1 "\n" %2)
          (map #(if (nil? %) "nil" (.toString %))
               [@mode @wire-p0 @cursor-pos @cursor-speed
                @catalog-pos @lels @selected-lels
                @wires @selected-wires @selected-name])))

;--------------------------------------------------
; text field
;--------------------------------------------------

(defn pane-text [f-set-to-parent str]
  (let [textfield (TextField. str)]
    (.setFocusTraversable textfield true)
    (f-set-to-parent textfield)
    textfield))

;--------------------------------------------------
; dialog box
;--------------------------------------------------

(defn dialog-table [type]
  (case type
    (in out) [['radio 'direction 'right 'up 'left 'down]]
    inout    [['radio 'direction 'horizontal 'vertical]]
    inv      [['radio 'direction 'right 'up 'left 'down]]
    (and or mux-n)
             [['edstr 'width  #(read-string %)]
              ['edstr 'height #(read-string %)]
              ['radio 'direction 'right 'up 'left 'down]]
    not      [['radio 'direction 'right 'up 'left 'down]]
    name     [['edstr 'string identity]
              ['radio 'h-align 'left   'center 'right]
              ['radio 'v-align 'bottom 'center 'top  ]]
    mux21    [['edstr 'width  #(read-string %)]
              ['edstr 'height #(read-string %)]
              ['radio 'direction 'right 'up 'left 'down]
              ['radio 'order01 :0->1 :1->0]]
    nil))

(defn prev-next [f list]
  (loop [prev nil l list]
    (cond (empty? l) [prev prev]
          (f (first l))
            [ (if prev prev (first l))
              (if (empty? (next l)) (first l) (fnext l))]
          :else (recur (first l) (next l))
          )))

(defn pane-dialog-cursor-move [cursors dir]
  (let [[prv nxt] (prev-next #(not= (.getFill %) Color/TRANSPARENT) cursors)
        target (case dir up prv, down nxt)]
    (doseq [c cursors]
      (.setFill c (if (= c target) Color/BLACK Color/TRANSPARENT))
      )))

(defn pane-dialog-radio-button-move [toggleGroup dir]
  (let [toggles (.getToggles toggleGroup)
        [prv nxt] (prev-next #(.isSelected %) toggles)]
    (.selectToggle toggleGroup (case dir left prv, right nxt))))

(defn pane-text-key-dialog [f-revert textfield label-on-dialog]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (when (#{KeyCode/ENTER KeyCode/ESCAPE} (.getCode keyEvent))
        (when (= (.getCode keyEvent) KeyCode/ENTER)
          (.setText label-on-dialog (.getText textfield)))
        (.consume keyEvent)
        (f-revert)
        ))))

(defn pane-dialog-revert [f-set-to-parent pane]
  (.setText *label-debug* (state-text))
  (f-set-to-parent pane)
  (.setFocusTraversable pane true)
  (.requestFocus pane))

(defn pane-dialog-key [f-set-to-parent f-revert pane rows lel-key]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [kc (.getCode keyEvent)]
        (cond (= KeyCode/ENTER kc)
                (dosync
                  (alter lels assoc lel-key
                   (into (@lels lel-key)
                    (map (fn [r]
                           [(:label r)
                            (case (:type r)
                              edstr ((:cast r) (.getText (:str r)))
                              radio ( symbol
                                      (.. (:togglegroup r)
                                          getSelectedToggle getText)))])
                         rows)))
                  (f-revert))
              (= KeyCode/ESCAPE kc)
                (f-revert)
              (= KeyCode/SPACE kc)
                (let [row (first (filter #(not= (.getFill (:cursor %))
                                                Color/TRANSPARENT)
                                         rows))]
                  (when (= (:type row) 'edstr)
                    (let [borderpane (BorderPane.)
                          textfield
                            (pane-text #(.setBottom borderpane %)
                                       (.getText (:str row)))]
                      (.setOnKeyPressed textfield
                       (pane-text-key-dialog
                        #(pane-dialog-revert f-set-to-parent pane)
                        textfield (:str row)))
                      (.setFocusTraversable pane false)
                      (.setCenter borderpane pane)
                      (f-set-to-parent borderpane)
                      (.setFocusTraversable textfield true)
                      (.requestFocus textfield))))
              (#{KeyCode/J KeyCode/K} kc)
                (pane-dialog-cursor-move (map #(:cursor %) rows)
                 (if (= kc KeyCode/J) 'down 'up))
              (#{KeyCode/H KeyCode/L} kc)
                (let [row (first ( filter
                                   #(not= (.getFill (:cursor %))
                                          Color/TRANSPARENT)
                                   rows))]
                  (when (= (:type row) 'radio)
                    (pane-dialog-radio-button-move
                     (:togglegroup row)
                     (if (= kc KeyCode/H) 'left 'right))))
              )))))

(defn pane-dialog [f-set-to-parent f-revert table lel-key]
  (let [lel (@lels lel-key)
        pane (VBox.)
        rows (map (fn [x]
                    (conj
                     {:type (x 0)
                      :cursor ( Polygon.
                                (double-array [0.0 0.0 10.0 5.0 0.0 10.0]))
                      :flowpane (FlowPane.)
                      :label (x 1)}
                     (case (first x)
                      edstr {:str (Label. (str (lel (x 1))))
                             :cast (x 2)}
                      radio {:togglegroup (ToggleGroup.)
                             :buttons
                               (map (fn [y] (RadioButton. (str y)))
                                    (drop 2 x)
                                    )})))
                  table)]
    (.setSpacing pane 8.0)
    (doseq [r rows]
      (.setFill (:cursor r) Color/TRANSPARENT)
      (.setVgap (:flowpane r) 12.0)
      (.setHgap (:flowpane r)  8.0)
      (.. (:flowpane r) getChildren (add (:cursor r)))
      (.. (:flowpane r) getChildren (add (Label. (str (:label r)))))
      (case (:type r)
        edstr (.. (:flowpane r) getChildren (add (:str r)))
        radio (doseq [button (:buttons r)]
                (.setToggleGroup button (:togglegroup r))
                (when (= (lel (symbol (:label r)))
                         (symbol (.getText button)))
                  (.selectToggle (:togglegroup r) button))
                (.. (:flowpane r) getChildren (add button))))
      (.. pane getChildren (add (:flowpane r))))
    (.setFill (:cursor (first rows)) Color/BLACK)
    (.setOnKeyPressed pane
                      (pane-dialog-key f-set-to-parent f-revert pane
                                       rows lel-key))
    (f-set-to-parent pane)
    pane))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(defn pane-schem-revert [f-set-to-parent pane]
  (.setText *label-debug* (state-text))
  (f-set-to-parent pane)
  (.setAll (.getChildren pane) (draw-mode))
  (.setFocusTraversable pane true)
  (.requestFocus pane))

(defn pane-schem-key [f-set-to-parent pane]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (cond (and (= KeyCode/D (.getCode keyEvent))
                 (= (:mode @mode) 'cursor))
            (let [lel-key (find-lel-by-pos @lels @cursor-pos)
                  dt (when lel-key
                       (dialog-table (get-in @lels [lel-key :type])))]
              (when dt
                (.setFocusTraversable pane false)
                (dosync (ref-set selected-name lel-key))
                (let [borderpane (BorderPane.)
                      dialog
                        ( pane-dialog #(.setRight borderpane %)
                          #(pane-schem-revert f-set-to-parent pane)
                          dt lel-key)]
                  (.setCenter borderpane pane)
                  (f-set-to-parent borderpane)
                  (.setFocusTraversable dialog true)
                  (.requestFocus dialog)
                  )))

            :else
            (let [f ((key-command (:mode @mode)) (.getCode keyEvent))]
              (when f
                (f 'dummy)
                (.consume keyEvent)
                (.setText *label-debug* (state-text))
                (.setAll (.getChildren pane) (draw-mode))
                ))))))

(defn pane-schem [f-set-to-parent]
  (let [pane (Pane.)]
    (.setOnKeyPressed pane (pane-schem-key f-set-to-parent pane))
    (.setFocusTraversable pane true)
    (.setAll (.getChildren pane) (draw-mode))
    (f-set-to-parent pane)
    pane))

;--------------------------------------------------
; Menus
;--------------------------------------------------
(defn my-file-chooser [main-stage title default-dir is-save]
  (let [fileChooser (FileChooser.)]
    (.setTitle fileChooser title)
    (when (and default-dir (.exists default-dir) (.isDirectory default-dir))
      (.setInitialDirectory fileChooser default-dir))
    (.. fileChooser getExtensionFilters
     (addAll
      (into-array FileChooser$ExtensionFilter
       [(FileChooser$ExtensionFilter. "RTL Schematica"
         (into-array String ["*.rtc"] ))
        (FileChooser$ExtensionFilter. "All Files"
         (into-array String ["*.*"]))])))
    (if is-save
      (.showSaveDialog fileChooser main-stage)
      (.showOpenDialog fileChooser main-stage))))

(let [prev-path (atom nil)]
  (defn action-open [main-stage]
    (proxy [EventHandler] []
      (handle [_]
        (let [file (my-file-chooser main-stage "Open File" @prev-path false)
              rd (when file (PushbackReader. (clojure.java.io/reader file)))]
          (when rd
            (dosync (doseq [r [lels wires]] (ref-set r (read rd))))
            (reset! prev-path (.getParentFile file))
            (.close rd)
            ))))))

(let [prev-path (atom nil)]
  (defn action-save-as [main-stage]
    (proxy [EventHandler] []
      (handle [_]
        (let [file (my-file-chooser main-stage "Save File As"
                                    @prev-path true)
              wr (when file (clojure.java.io/writer file))]
          (when wr
            (doseq [x [@lels @wires]]
              (clojure.pprint/pprint x wr))
            (reset! prev-path (.getParentFile file))
            (.close wr)
            ))))))

(defn action-exit []
  (proxy [EventHandler] []
    (handle [_]
      (System/exit 0))))

(defn pane-about [f-revert]
  (let [label (Label. (str "RTL Schematica ver. 0.0.0\n"
                           "Authored by Tackya Yammouch"))
        ok-button (Button. "OK")
        pane (VBox.)]
    (.setDefaultButton ok-button true)
    (.setOnAction ok-button
     (proxy [EventHandler] []
       (handle [_] (f-revert))))
    (.. pane getChildren
     (setAll
      (into-array Node [label ok-button])))
    pane))

(defn action-about [f-set-to-parent f-revert]
  (proxy [EventHandler] []
    (handle [_]
      (f-set-to-parent (pane-about f-revert))
      )))

(defn menu-top [main-stage f-set-to-parent f-revert]
  (let [menu (MenuBar.)
        file (Menu. "File")
        open (MenuItem. "Open")
        save-as (MenuItem. "Save As")
        exit (MenuItem. "Exit")
        help (Menu. "help")
        about (MenuItem. "About")]
    (.setOnAction open (action-open main-stage))
    (.setOnAction save-as (action-save-as main-stage))
    (.setOnAction exit (action-exit))
    (.setOnAction about (action-about f-set-to-parent f-revert))
    (doseq [x [open save-as exit]]
      (.. file getItems (add x)))
    (.. help getItems (add about))
    (doseq [x [file help]]
      (.. menu getMenus (add x)))
    (.setFocusTraversable menu true)
    menu))

;--------------------------------------------------
; JavaFX main routine
;--------------------------------------------------
(defn -start [self stage]
  (let [topgroup (BorderPane.)
        pane (pane-schem #(.setCenter topgroup %))
        menu (menu-top stage
                       #(.setCenter topgroup %)
                       #(.setCenter topgroup pane))]
    (.setWrapText *label-debug* true)
    (.setTop topgroup menu)
    (.setBottom topgroup *label-debug*)
    (.setText *label-debug* (state-text))
    (doto stage
      (.setWidth 1024) (.setHeight 768)
      (.setScene (Scene. topgroup))
      (.setTitle "Shows Some Gates")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "SchemRtl")
                      (into-array String [])))

