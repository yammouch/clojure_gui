(ns LogicElementsDraw)

(gen-class
  :name "LogicElementsDraw")

(import
  '(javafx.geometry    VPos)
  '(javafx.scene       Node)
  '(javafx.scene.paint Color)
  '(javafx.scene.shape Rectangle Polygon Polyline Line Circle
                       Path PathElement MoveTo ArcTo LineTo ClosePath)
  '(javafx.scene.text  Font Text TextAlignment))

(require 'LogicElements)
(alias 'lel 'LogicElements)

(def pix-per-grid 8.0)
(defn grid2screen [[grid-x grid-y]]
  [(* pix-per-grid grid-x)
   (* pix-per-grid grid-y)])

;--------------------------------------------------
; rotate
;--------------------------------------------------
;(defn rotate [[x y] degree] ; clockwise, because y gets larger downward
;  (case degree
;    0   [   x     y ]
;    90  [(- y)    x ]
;    180 [(- x) (- y)]
;    270 [   y  (- x)]))
(defn rotate-ofs [[x y] width height degree]
  "rotate a point fixing the upper-left corner"
  (case degree
    (  0 :right :horizontal) [             x                y  ]
    ( 90 :down             ) [(+ height (- y))              x  ]
    (180 :left             ) [(+ width  (- x)) (+ height (- y))]
    (270 :up    :vertical  ) [             y   (+ width  (- x))]
    ))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-text [[x y] str color v-align h-align]
  (let [text (Text. str)]
    (doto text
      (.setFont (Font. "monospaced Regular" 12.0))
      (.setY (+ (* y pix-per-grid)
                (case v-align :bottom -2.0, :center 0.0, :top 2.0)))
      (.setTextOrigin (case v-align
                        :bottom VPos/BOTTOM
                        :center VPos/CENTER
                        :top    VPos/TOP))
      (.setStroke color))
    (let [width (.. text getLayoutBounds getWidth)]
      (.setX text (+ (* x pix-per-grid)
                     (case h-align
                       :left   (* 0.5 pix-per-grid)
                       :center (- (* 0.5 width))
                       :right  (- (* 0.5 pix-per-grid) width)
                       ))))
    text))

(defn draw-dot [[x y] size color]
  (Circle. (* x pix-per-grid)
           (* y pix-per-grid)
           (* 0.5 size) color))


(defn draw-rect [[[x0 y0] [x1 y1]] color]
  (let [x (* pix-per-grid (min x0 x1))
        y (* pix-per-grid (min y0 y1))
        w (* pix-per-grid (Math/abs (- x1 x0)))
        h (* pix-per-grid (Math/abs (- y1 y0)))
        rect (Rectangle. x y w h)]
    (doto rect
      (.setStroke color) (.setFill Color/TRANSPARENT))
    rect))

(defn draw-wire [[[x0 y0] [x1 y1]] color]
  (let [line (Line. (* x0 pix-per-grid)
                    (* y0 pix-per-grid)
                    (* x1 pix-per-grid)
                    (* y1 pix-per-grid))]
    (.setStroke line color)
    line))

(defn draw-wire-selected [[[x0 y0] [x1 y1]] selected]
  (if (= selected #{0 1})
    (let [line (apply #(Line. %1 %2 %3 %4)
                (mapcat grid2screen [[x0 y0] [x1 y1]]))]
      (.setStroke line Color/RED)
      [line])
    (let [x- (- x1 x0) y- (- y1 y0)
          len (Math/sqrt (+ (* x- x-) (* y- y-)))

          [xorg xhl yorg yhl] ; hl: highlight
          (if (= selected #{0})
            [x0 (+ x0 (/ x- len)) y0 (+ y0 (/ y- len))]
            [x1 (- x1 (/ x- len)) y1 (- y1 (/ y- len))])
          shortline (apply #(Line. %1 %2 %3 %4)
                     (mapcat grid2screen [[xorg yorg] [xhl yhl]]))
          longline (apply #(Line. %1 %2 %3 %4)
                    (mapcat grid2screen
                            [[xhl yhl] (if (= selected #{0})
                                         [x1 y1] [x0 y0])]))]
      (.setStroke shortline Color/RED)
      [shortline longline])))

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------
(defmulti lel-draw (fn [lel color & xs] (:type lel)))

(defn unidirectional-port-symbol [{[x y] :p d :direction :as lel} color]
  (let [symbol (Polygon. (double-array (apply concat
                (map #(grid2screen (map + (rotate-ofs % 3 2 d) [x y]))
                     [[0 0] [2 0] [3 1] [2 2] [0 2]]
                     ))))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text [(+ x (* 0.5 (lel/width  lel)))
                 (+ y (* 0.5 (lel/height lel)))]
                (case (:type lel) :in "I", :out "O")
                color :center :center)
     symbol]))

; for "in"
(defmethod lel-draw :in [lel color] (unidirectional-port-symbol lel color))

; for "out"
(defmethod lel-draw :out [lel color] (unidirectional-port-symbol lel color))

; for "inout"
(defmethod lel-draw :inout [{[x y] :p d :direction :as lel} color]
  (let [symbol (Polygon. (double-array
                (mapcat #(grid2screen (map + (rotate-ofs % 3 2 d) [x y]))
                        [[0 1] [1 0] [2 0] [3 1] [2 2] [1 2]]
                        )))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text [(+ x (* 0.5 (lel/width  lel)))
                 (+ y (* 0.5 (lel/height lel)))]
                "IO" color :center :center)
     symbol]))

; for "dot"
(defmethod lel-draw :dot [lel color] [(draw-dot (:p lel) 7 color)])

; for "name"
(defmethod lel-draw :name [lel color]
  (let [[x y] (grid2screen (:p lel))
        line-h (Line. (- x 1.0) y (+ x 1.0) y)
        line-v (Line. x (- y 1.0) x (+ y 1.0))]
    (.setStroke line-h color) (.setStroke line-v color)
    [(draw-text (:p lel) (lel :string) color
                (lel :v-align) (lel :h-align))
     line-h line-v]))

(defmethod lel-draw :not [{[x y] :p d :direction :as lel} color]
  (let [[cx cy] (grid2screen (map + (rotate-ofs [0.5 0] 0 0 d) [x y]))
        circle (Circle. cx cy (* 0.5 pix-per-grid))]
    (doto circle   (.setStroke color) (.setFill Color/TRANSPARENT))
    [circle]))

; for "buf"
(defmethod lel-draw :buf
  [{[x y] :p d :direction w :width h :height :as lel} color]
  (let [[[x0 y0] [x1 y1] [x2 y2]]
        (map #(grid2screen (map + (rotate-ofs % w h d) [x y]))
             [[0 0] [w (* 0.5 h)] [0 h]])
        triangle (Polygon. (double-array [x0 y0 x1 y1 x2 y2]))]
    (doto triangle (.setStroke color) (.setFill Color/TRANSPARENT))
    [triangle]))

; for "and"
(defmethod lel-draw :and
  [{[x y] :p d :direction w :width h :height :as lel} color]
  (let [[[x0 y0] [x1 y1] [x2 y2] [x3 y3]]
        (map #(grid2screen (map + (rotate-ofs % w h d) [x y]))
             [[0 0] [(* 0.5 w) 0] [(* 0.5 w) h] [0 h]])
        [rx ry] (map #(* 0.5 pix-per-grid %) (case d (:up :down) [h w] [w h]))
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
(defmethod lel-draw :or
  [{[x y] :p d :direction w :width h :height :as lel} color]
  (let [[[x0 y0] [x1 y1]]
        (map #(grid2screen (map + (rotate-ofs % w h d) [x y]))
             [[0 0] [0 h]])
        [rx0 ry0 rx1 ry1]
        (map #(* pix-per-grid %)
         (case d (:right :left)
           [       w  (* 0.5 h) (* 0.25 w) (* 0.5  h)]
           [(* 0.5 h)        w  (* 0.5  h) (* 0.25 w)]))
        symbol (Path. (into-array PathElement
                [(MoveTo. x0 y0)
                 (ArcTo. rx0 ry0 0.0 x1 y1 false true)
                 (ArcTo. rx1 ry1 0.0 x0 y0 false false)
                 (ClosePath.)
                 ]))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [symbol]))

; for "dff"
(defmethod lel-draw :dff [{[x y] :p w :width h :height :as lel} color]
  (let [[rectx recty] (grid2screen (:p lel))
        rect (Rectangle. rectx recty (* w pix-per-grid) (* h pix-per-grid))
        line (Polyline. (double-array
              (mapcat #(grid2screen (map + % [x y]))
                      [[(- (* 0.5 w) 1)    h   ]
                       [(   * 0.5 w   ) (- h 1)]
                       [(+ (* 0.5 w) 1)    h   ]])))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto line (.setStroke color))
    (if (= (lel :async-reset) :true)
      [rect line (draw-text [(+ x (* 0.5 w)) y] "R" color :top :center)]
      [rect line])))

; for "mux21"
(defmethod lel-draw :mux21
  [{[x y] :p d :direction w :width h :height :as lel} color]
  (concat
   (lel-draw (assoc lel :type :mux-n) color)
   (let [[[ax ay] [bx by]] (map #(map + (rotate-ofs % w h d) [x y])
                                [[(* 0.5 w) 2] [(* 0.5 w) (- h 2)]]
                                )]
     [(draw-text [ax ay] (if (= (lel :order01) :0->1) "0" "1")
                 color :center :center)
      (draw-text [bx by] (if (= (lel :order01) :0->1) "1" "0")
                 color :center :center
                 )])))

; for "mux-n"
(defmethod lel-draw :mux-n
  [{[x y] :p d :direction w :width h :height :as lel} color]
  (let [trapezoid (Polygon. (double-array
                   (mapcat #(grid2screen (map + (rotate-ofs % w h d) [x y]))
                           [[0 0] [w 2] [w (- h 2)] [0 h]]
                           )))]
    (doto trapezoid (.setStroke color) (.setFill Color/TRANSPARENT))
    [trapezoid]))

; for "op"
(defmethod lel-draw :op [{[x y] :p w :width h :height :as lel} color]
  (let [[rectx recty] (grid2screen (:p lel))
        rect (Rectangle. rectx recty (* w pix-per-grid) (* h pix-per-grid))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text [(+ x (* 0.5 w)) (+ y (* 0.5 w))] (:operator lel)
                color :center :center)
     rect]))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor
  [{:keys [mode cursor-pos lels selected-lels geoms selected-geoms rect-p0]}]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (mapcat (fn [[k {type :type p :p}]]
              (case type
                :wire (let [selected (selected-geoms k)]
                        (if selected
                          (draw-wire-selected p selected)
                          [(draw-wire p Color/BLACK)]))
                :rect [(draw-rect p Color/BLACK)]))
            geoms)
    (mapcat (fn [[k v]]
              (lel-draw v (if (selected-lels k) Color/RED Color/BLACK)))
            lels)
    (when rect-p0
      (let [x (Math/min (cursor-pos 0) (rect-p0 0))
            y (Math/min (cursor-pos 1) (rect-p0 1))
            width  (Math/abs (- (cursor-pos 0) (rect-p0 0)))
            height (Math/abs (- (cursor-pos 1) (rect-p0 1)))
            rect (Rectangle. (* x pix-per-grid)
                             (* y pix-per-grid)
                             (* width pix-per-grid)
                             (* height pix-per-grid))]
        (.setFill rect Color/TRANSPARENT)
        (.setStroke rect Color/BLACK)
        (.setAll (.getStrokeDashArray rect)
                 (into-array Double [2.0 2.0]))
        [rect])))))

(defn draw-mode-move
  [{:keys [cursor-pos lels moving-lels geoms moving-geoms moving-vertices]}]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (mapcat (fn [[k {p :p}]]
              (let [vertices (moving-vertices k)]
                (if vertices
                  (draw-wire-selected p vertices)
                  [(draw-wire p Color/BLACK)])))
            geoms)
    (mapcat (fn [[_ v]] (lel-draw v Color/BLACK)) lels)
    (map (fn [[_ {p :p}]] (draw-wire p Color/RED)) moving-geoms)
    (mapcat (fn [[_ v]] (lel-draw v Color/RED)) moving-lels)
    )))

(defn draw-mode-add [{:keys [mode cursor-pos lels geoms] :as schem}]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (map (fn [[_ {p :p}]] (draw-wire p Color/BLACK)) geoms)
    (mapcat (fn [[_ v]] (lel-draw v Color/BLACK)) lels)
    (lel-draw (conj (:lel schem) cursor-pos)
              Color/RED))))

(defn draw-mode-wire [{:keys [cursor-pos lels geoms wire-p0]}]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (map (fn [[_ {p :p}]] (draw-wire p Color/BLACK)) geoms)
    (mapcat (fn [[_ v]] (lel-draw v Color/BLACK)) lels)
    [(draw-wire [wire-p0 cursor-pos] Color/RED)])))

(def catalog-table
  [[:in    :out   :inout :dot :not ]
   [:buf   :and   :or    :dff :name]
   [:mux21 :mux-n :op              ]])

(defn draw-mode-catalog [{catalog-pos :catalog-pos}]
  (let [parts (mapcat (fn [idx0 parts]
                        (map (fn [idx1 part]
                               {:idx0 idx0, :idx1 idx1, :part part})
                             (range) parts))
                      (range) catalog-table)
        rect (Rectangle. (* pix-per-grid (+ (* 10 (catalog-pos 0)) 1))
                         (* pix-per-grid (+ (* 10 (catalog-pos 1)) 1))
                         (* pix-per-grid 10)
                         (* pix-per-grid 10))]
    (.setStroke rect Color/RED)
    (.setStrokeWidth rect 2.0)
    (.setFill rect Color/TRANSPARENT)
    (into-array Node
     (concat
      (mapcat (fn [{idx0 :idx0 idx1 :idx1 part :part}]
                (let [lel (lel/lel-init part)]
                  (lel-draw (conj lel
                                  {:x (- (+ (* 10 idx1) 6)
                                         (int (/ (lel/width lel) 2)))
                                   :y (- (+ (* 10 idx0) 6)
                                         (int (/ (lel/height lel) 2))
                                         )})
                            Color/BLACK)))
              parts)
      [rect]))))

