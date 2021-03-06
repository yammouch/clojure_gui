(ns schem-rtl
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font BasicStroke BorderLayout])
(import '[java.awt.font TextLayout])
(import '[javax.swing JFrame JPanel JOptionPane JTextArea KeyStroke
                      AbstractAction])
(import '[java.awt.event KeyListener KeyEvent])
(require 'clojure.set)

(def pix-per-grid 8)

(defn -init []
  [[] (atom [])])

;--------------------------------------------------
; states
;--------------------------------------------------

(def lels
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               '[{:y 20, :type name , :x 5 ,
                  :str "hoge", :v-align b, :h-align l}
                 {:y 28, :type in   , :x 25}
                 {:y 22, :type in   , :x 25}
                 {:y 28, :type and  , :x 32}
                 {:y 23, :type or   , :x 40}
                 {:y 30, :type in   , :x 25}
                 {:y 36, :type in   , :x 25}
                 {:y 26, :type out  , :x 62}
                 {:y 34, :type in   , :x 25}
                 {:y 22, :type and  , :x 32}
                 {:y 29, :type dot  , :x 30}
                 {:y 26, :type dff  , :x 55}
                 {:y 24, :type mux21, :x 48}
                 ])))

(def selected-lels (ref #{}))
(def selected-wires (ref #{}))
(def selected-name (ref nil))

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

(def cursor-pos (ref {:x 5 :y 5}))
(def cursor-speed (ref 1))
(def mode (ref {:mode 'cursor}))
(def wire-p0 (ref {:x 0 :y 0}))
(def catalog-pos (ref {:x 0 :y 0}))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-text [g pos str color font v-align h-align]
  (let [frc (.getFontRenderContext g)
        bound (.getBounds (TextLayout. str font frc))
        x (int (- (* (pos :x) pix-per-grid)
                  (case h-align
                    l 0
                    c (* 0.5 (.getWidth bound))
                    r (.getWidth bound))))
        y (int (+ (* (pos :y) pix-per-grid)
                  (case v-align
                    b 0
                    m (* 0.5 (.getHeight bound))
                    t (.getHeight bound))))]
    (.setColor g color)
    (.setFont g font)
    (.drawString g str x y)))

(defn draw-dot [g pos size color]
  (let [x (- (* (pos :x) pix-per-grid)
             (int (* 0.5 size)))
        y (- (* (pos :y) pix-per-grid)
             (int (* 0.5 size)))]
    (.setColor g color)
    (.fillOval g x y size size)))

(let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
  (defn draw-status [g objs]
    (.setColor g Color/BLUE)
    (.setFont g font)
    (doseq [[obj ypos] (map #(list (if (nil? %1) "nil" %1)
                                   (+ 12 (* 12 %2)))
                            objs (range))]
      (.drawString g (.toString obj) 2 ypos))))

(defn draw-wire [g {x0 :x0 y0 :y0 x1 :x1 y1 :y1} color]
  (.setColor g color)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid %) [x0 x1]))
                 (int-array (map #(* pix-per-grid %) [y0 y1]))
                 2))

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------

(defn lel-init [type]
  (case type
    in    {:type 'in    :x 0 :y 0}
    out   {:type 'out   :x 0 :y 0}
    inout {:type 'inout :x 0 :y 0}
    dot   {:type 'dot   :x 0 :y 0}
    name  {:type 'name  :x 0 :y 0 :str "blah" :v-align 'b :h-align 'l}
    not   {:type 'not   :x 0 :y 0}
    and   {:type 'and   :x 0 :y 0}
    or    {:type 'or    :x 0 :y 0}
    dff   {:type 'dff   :x 0 :y 0}
    mux21 {:type 'mux21 :x 0 :y 0}
    plus  {:type 'plus  :x 0 :y 0}
    minus {:type 'minus :x 0 :y 0}
    ))

(defmulti lel-width  (fn [lel] (:type lel)))
(defmulti lel-height (fn [lel] (:type lel)))
(defmulti lel-x-min  (fn [lel] (:type lel)))
(defmulti lel-x-max  (fn [lel] (:type lel)))
(defmulti lel-y-min  (fn [lel] (:type lel)))
(defmulti lel-y-max  (fn [lel] (:type lel)))
(defmulti lel-draw   (fn [lel g color & xs] (:type lel)))

; Following declarations look redundant at this commit.
; But width and height will be variables after adding some size change
; features.

; for "in"
(defmethod lel-width  'in [lel] 3)
(defmethod lel-height 'in [lel] 2)
(defmethod lel-x-min  'in [lel] (:x lel))
(defmethod lel-x-max  'in [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'in [lel] (:y lel))
(defmethod lel-y-max  'in [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'in [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 2 3 2 0]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 0 1 2 2]))
                5)
  (draw-text g {:x (+ (lel :x) 1.5) :y (+ (lel :y) 1)}
             "I" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

; for "out"
(defmethod lel-width  'out [lel] 3)
(defmethod lel-height 'out [lel] 2)
(defmethod lel-x-min  'out [lel] (:x lel))
(defmethod lel-x-max  'out [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'out [lel] (:y lel))
(defmethod lel-y-max  'out [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'out [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 2 3 2 0]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 0 1 2 2]))
                5)
  (draw-text g {:x (+ (lel :x) 1.5) :y (+ (lel :y) 1)}
             "O" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

; for "inout"
(defmethod lel-width  'inout [lel] 3)
(defmethod lel-height 'inout [lel] 2)
(defmethod lel-x-min  'inout [lel] (:x lel))
(defmethod lel-x-max  'inout [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'inout [lel] (:y lel))
(defmethod lel-y-max  'inout [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'inout [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 1 2 3 2 1]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [1 0 0 1 2 2]))
                6)
  (draw-text g {:x (+ (lel :x) 1.5) :y (+ (lel :y) 1)}
             "IO" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

; for "dot"
(defmethod lel-width  'dot [lel] 0)
(defmethod lel-height 'dot [lel] 0)
(defmethod lel-x-min  'dot [lel] (:x lel))
(defmethod lel-x-max  'dot [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'dot [lel] (:y lel))
(defmethod lel-y-max  'dot [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'dot [lel g color]
  (draw-dot g lel 7 color))

; for "name"
; 0 of size is feasible?
(defmethod lel-width  'name [lel] 0)
(defmethod lel-height 'name [lel] 0)
(defmethod lel-x-min  'name [lel] (:x lel))
(defmethod lel-x-max  'name [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'name [lel] (:y lel))
(defmethod lel-y-max  'name [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'name [lel g color]
  (draw-text g lel (:str lel) color
             (Font. Font/MONOSPACED Font/PLAIN 12)
             (:v-align lel) (:h-align lel))
  (let [x (* (:x lel) pix-per-grid)
        y (* (:y lel) pix-per-grid)]
    (.setColor g color)
    (.drawPolyline g
                   (int-array [(dec x) (inc x)])
                   (int-array [y y])
                   2)
    (.drawPolyline g
                   (int-array [x x])
                   (int-array [(dec y) (inc y)])
                   2)))

; for "not"
(defmethod lel-width  'not [lel] 3)
(defmethod lel-height 'not [lel] 4)
(defmethod lel-x-min  'not [lel] (:x lel))
(defmethod lel-x-max  'not [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'not [lel] (:y lel))
(defmethod lel-y-max  'not [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'not [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 2 0]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 2 4]))
                3)
  (.drawOval g
             (int (* (+ (lel :x) 2  ) pix-per-grid))
             (int (* (+ (lel :y) 1.5) pix-per-grid))
             pix-per-grid
             pix-per-grid))

; for "and"
; "and" should be extended according to needed inputs.
(defmethod lel-width  'and [lel] 4)
(defmethod lel-height 'and [lel] 4)
(defmethod lel-x-min  'and [lel] (:x lel))
(defmethod lel-x-max  'and [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'and [lel] (:y lel))
(defmethod lel-y-max  'and [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'and [lel g color]
  (.setColor g color)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                 [2 0 0 2]))
                 (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                 [0 0 4 4]))
                 4)
  (.drawArc g
            (int (* (lel :x) pix-per-grid))
            (int (* (lel :y) pix-per-grid))
            (* 4 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180))

; for "or"
; "or" should be extended according to needed inputs.
(defmethod lel-width  'or [lel] 4)
(defmethod lel-height 'or [lel] 4)
(defmethod lel-x-min  'or [lel] (:x lel))
(defmethod lel-x-max  'or [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'or [lel] (:y lel))
(defmethod lel-y-max  'or [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'or [lel g color]
  (.setColor g color)
  (.drawArc g
            (int (* (- (lel :x) 1) pix-per-grid))
            (int (* (lel :y) pix-per-grid))
            (* 2 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180)
  (.drawArc g
            (int (* (- (lel :x) 4) pix-per-grid))
            (int (* (lel :y) pix-per-grid))
            (* 8 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180))

; for "dff"
(defmethod lel-width  'dff [lel] 4)
(defmethod lel-height 'dff [lel] 5)
(defmethod lel-x-min  'dff [lel] (:x lel))
(defmethod lel-x-max  'dff [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'dff [lel] (:y lel))
(defmethod lel-y-max  'dff [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'dff [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 0 4 4]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 5 5 0]))
                4)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                 [1 2 3]))
                 (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                 [5 4 5]))
                 3))

; for "mux21"
(defmethod lel-width  'mux21 [lel] 2)
(defmethod lel-height 'mux21 [lel] 6)
(defmethod lel-x-min  'mux21 [lel] (:x lel))
(defmethod lel-x-max  'mux21 [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'mux21 [lel] (:y lel))
(defmethod lel-y-max  'mux21 [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'mux21 [lel g color]
  (let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
    (draw-text g {:x (+ (lel :x) 1) :y (+ (lel :y) 2)}
               "0" color font 'm 'c)
    (draw-text g {:x (+ (lel :x) 1) :y (+ (lel :y) 4)}
               "1" color font 'm 'c)
    (.setColor g color)
    (.drawPolygon g
                  (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                  [0 2 2 0]))
                  (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                  [0 2 4 6]))
                  4)))

; for "plus"
(defmethod lel-width  'plus [lel] 4)
(defmethod lel-height 'plus [lel] 4)
(defmethod lel-x-min  'plus [lel] (:x lel))
(defmethod lel-x-max  'plus [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'plus [lel] (:y lel))
(defmethod lel-y-max  'plus [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'plus [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 4 4 0]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 0 4 4]))
                4)
  (draw-text g {:x (+ (lel :x) 2) :y (+ (lel :y) 2)}
             "+" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

; for "minus"
(defmethod lel-width  'minus [lel] 4)
(defmethod lel-height 'minus [lel] 4)
(defmethod lel-x-min  'minus [lel] (:x lel))
(defmethod lel-x-max  'minus [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  'minus [lel] (:y lel))
(defmethod lel-y-max  'minus [lel] (+ (:y lel) (lel-height lel)))
(defmethod lel-draw   'minus [lel g color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (lel :x) %))
                                [0 4 4 0]))
                (int-array (map #(* pix-per-grid (+ (lel :y) %))
                                [0 0 4 4]))
                4)
  (draw-text g {:x (+ (lel :x) 2) :y (+ (lel :y) 2)}
             "-" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor [g]
  (draw-dot g @cursor-pos 9 Color/BLUE)
  (doseq [[k v] @wires]
    (draw-wire g v (if (@selected-wires k) Color/RED Color/BLACK)))
  (doseq [[k v] @lels]
    (lel-draw v g (if (@selected-lels k) Color/RED Color/BLACK)))
  (when (@mode :rect-x0)
    (.setStroke g
                (BasicStroke. 1.0
                              BasicStroke/CAP_BUTT
                              BasicStroke/JOIN_BEVEL
                              1.0 (float-array [2.0]) 0.0))
    (let [x (Math/min (@cursor-pos :x) (@mode :rect-x0))
          y (Math/min (@cursor-pos :y) (@mode :rect-y0))
          width  (Math/abs (- (@cursor-pos :x) (@mode :rect-x0)))
          height (Math/abs (- (@cursor-pos :y) (@mode :rect-y0)))]
      (when (and (< 0 width) (< 0 height))
        (.drawRect g (int (* x pix-per-grid))
                     (int (* y pix-per-grid))
                     (int (* width  pix-per-grid))
                     (int (* height pix-per-grid))
                     )))))

(defn draw-mode-add [g]
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (lel-draw v g Color/BLACK))
  (lel-draw (conj {:type (:type @mode)}
                  @cursor-pos
                  (when (= (:type @mode) 'name) ; to be refactored
                    {:str "blah" :v-align 'b :h-align 'l}
                    ))
            g Color/RED))

(defn draw-mode-wire [g]
  (draw-dot g @cursor-pos 9 Color/BLUE)
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (lel-draw v g Color/BLACK))
  (draw-wire g
             {:x0 (@wire-p0 :x) :y0 (@wire-p0 :y)
              :x1 (@cursor-pos :x) :y1 (@cursor-pos :y)}
             Color/RED))

(def catalog-table
  '[[in   out inout dot name ]
    [not  and or    dff mux21]
    [plus minus]])

(defn draw-mode-catalog [g]
  (doseq [[idx0 parts] (map #(list %1 %2) (range) catalog-table)]
    (doseq [[idx1 part] (map #(list %1 %2) (range) parts)]
      (let [lel (lel-init part)]
        (lel-draw (conj lel
                        {:x (- (+ (* 10 idx1) 6)
                               (int (/ (lel-width lel) 2)))
                         :y (- (+ (* 10 idx0) 6)
                               (int (/ (lel-height lel) 2))
                               )})
                  g Color/BLACK))))
  (.setStroke g (BasicStroke. 2.0))
  (.setColor g Color/RED)
  (.drawRect g (* pix-per-grid (+ (* 10 (@catalog-pos :x)) 1))
               (* pix-per-grid (+ (* 10 (@catalog-pos :y)) 1))
               (* pix-per-grid 10)
               (* pix-per-grid 10)))

;--------------------------------------------------
; drawing on Java GUI
;--------------------------------------------------

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-status g [@cursor-pos @cursor-speed @mode
                      @lels @selected-lels
                      @wires @selected-wires @catalog-pos
                      @selected-name])
      (case (@mode :mode)
        cursor  (draw-mode-cursor  g)
        move    (draw-mode-cursor  g)
        add     (draw-mode-add     g)
        wire    (draw-mode-wire    g)
        catalog (draw-mode-catalog g)))
    (getPreferredSize []
      (Dimension. 800 400))))

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

(defn move-wire [wire dir speed]
  (let [[f & keys] (case dir
                     left  [#(- % speed) :x0 :x1]
                     right [#(+ % speed) :x0 :x1]
                     up    [#(- % speed) :y0 :y1]
                     down  [#(+ % speed) :y0 :y1])]
    (reduce (fn [wire k] (assoc wire k (f (wire k))))
            wire keys)))

(defn move-selected-wires [dir speed]
  (let [moved (reduce (fn [wires sel]
                        (assoc wires sel
                               (move-wire (wires sel) dir speed)))
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
    (ref-set selected-wires #{})))

(defn find-lel-by-pos [lels pos]
  (some (fn [[k v]]
          (when (and (= (pos :x) (v :x))
                     (= (pos :y) (v :y)))
            k))
        lels))

(defn find-wire-by-pos [wires pos]
  (some (fn [[k {x0 :x0 y0 :y0 x1 :x1 y1 :y1}]]
          (when (or (and (= (pos :x) x0)
                         (= (pos :y) y0))
                    (and (= (pos :x) x1)
                         (= (pos :y) y1)))
            k))
        wires))

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
    {:lels (set (keys lels)) :wires (set (keys wires))}
    ))

; Function name should be generalized? Wires also can be removed by it.
(defn remove-lel-by-key [lels keys]
  (apply hash-map
         (apply concat
                (remove (fn [[k v]] (keys k))
                        lels))))

(defn close-window [frame]
  (let [yn (JOptionPane/showConfirmDialog
            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
    (when (= yn JOptionPane/YES_OPTION)
      (.dispose frame))))

;--------------------------------------------------
; key commands for each mode on schematic panel
;--------------------------------------------------

(def key-command-cursor-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_I      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< @cursor-speed 64)
                                    (* 2 @cursor-speed)
                                    64))))
   KeyEvent/VK_U      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< 1 @cursor-speed)
                                    (/ @cursor-speed 2)
                                    1))))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_C      (fn [_] (dosync (ref-set mode {:mode 'catalog})))
   KeyEvent/VK_A      (fn [_] (dosync
                                (release-selection)
                                (ref-set mode {:mode 'dff})))
   KeyEvent/VK_B      (fn [_] (dosync
                                (release-selection)
                                (ref-set mode {:mode 'mux21})))
   KeyEvent/VK_M      (fn [_] (dosync
                                (ref-set mode {:mode 'move})))
   KeyEvent/VK_W      (fn [_] (dosync
                                (release-selection)
                                (ref-set wire-p0 @cursor-pos)
                                (ref-set mode {:mode 'wire})))
   KeyEvent/VK_R
   (fn [_]
     (if (:rect-x0 @mode)
       (dosync (alter mode dissoc :rect-x0 :rect-y0))
       (dosync (alter mode conj {:rect-x0 (@cursor-pos :x)
                                 :rect-y0 (@cursor-pos :y)}))))
   KeyEvent/VK_ENTER
   (fn [_]
     (let [lel-key (find-lel-by-pos @lels @cursor-pos)
           wire-key (when (not lel-key)
                      (find-wire-by-pos @wires @cursor-pos))
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
           (alter selected-wires conj wire-key))
         (when (:wires rect-keys)
           (alter selected-wires clojure.set/union (:wires rect-keys)))
         (alter mode dissoc :rect-x0 :rect-y0)
         )))
   KeyEvent/VK_T
   (fn [{text-area :text-area}]
     (let [lel-key (find-lel-by-pos @lels @cursor-pos)]
       (when (= (:type (@lels lel-key)) 'name)
         (dosync (ref-set selected-name lel-key))
         (.setText text-area (:str (@lels lel-key)))
         (.requestFocus text-area)
         )))
   KeyEvent/VK_ESCAPE (fn [_]
                        (dosync (alter mode dissoc :rect-x0 :rect-y0))
                        (release-selection))
   KeyEvent/VK_X      (fn [_] (dosync
                                (alter lels remove-lel-by-key @selected-lels)
                                (alter wires remove-lel-by-key @selected-wires)
                                (ref-set selected-lels #{})
                                ))})

; add mode can be merged into move mode
; if continuous addition is not necessary.
(def key-command-add-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_I      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< @cursor-speed 64)
                                    (* 2 @cursor-speed)
                                    64))))
   KeyEvent/VK_U      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< 1 @cursor-speed)
                                    (/ @cursor-speed 2)
                                    1))))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_C      (fn [_] (dosync (ref-set mode {:mode 'catalog})))
   KeyEvent/VK_ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym)
               (conj (lel-init (:type @mode))
                     @cursor-pos)})))
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   })

(def key-command-move-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor   'left  @cursor-speed)
                              (move-selected 'left  @cursor-speed))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor   'right @cursor-speed)
                              (move-selected 'right @cursor-speed))
   KeyEvent/VK_UP     (fn [_] (move-cursor   'up    @cursor-speed)
                              (move-selected 'up    @cursor-speed))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor   'down  @cursor-speed)
                              (move-selected 'down  @cursor-speed))
   KeyEvent/VK_H      (fn [_] (move-cursor   'left  @cursor-speed)
                              (move-selected 'left  @cursor-speed))
   KeyEvent/VK_L      (fn [_] (move-cursor   'right @cursor-speed)
                              (move-selected 'right @cursor-speed))
   KeyEvent/VK_K      (fn [_] (move-cursor   'up    @cursor-speed)
                              (move-selected 'up    @cursor-speed))
   KeyEvent/VK_J      (fn [_] (move-cursor   'down  @cursor-speed)
                              (move-selected 'down  @cursor-speed))
   KeyEvent/VK_I      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< @cursor-speed 64)
                                    (* 2 @cursor-speed)
                                    64))))
   KeyEvent/VK_U      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< 1 @cursor-speed)
                                    (/ @cursor-speed 2)
                                    1))))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ESCAPE (fn [_] (dosync
                                (release-selection)
                                (ref-set mode {:mode 'cursor})))
                                })

(def key-command-wire-mode  
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left  @cursor-speed))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right @cursor-speed))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up    @cursor-speed))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down  @cursor-speed))
   KeyEvent/VK_I      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< @cursor-speed 64)
                                    (* 2 @cursor-speed)
                                    64))))
   KeyEvent/VK_U      (fn [_] (dosync
                                (ref-set cursor-speed
                                  (if (< 1 @cursor-speed)
                                    (/ @cursor-speed 2)
                                    1))))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ESCAPE (fn [_] (dosync
                                (ref-set mode {:mode 'cursor})))
   KeyEvent/VK_ENTER  (fn [_] (dosync
                                (alter wires conj
                                      {(gensym) {:x0 (@wire-p0 :x)
                                                 :y0 (@wire-p0 :y)
                                                 :x1 (@cursor-pos :x)
                                                 :y1 (@cursor-pos :y)}})
                                (ref-set mode {:mode 'cursor})))
                                })

(def key-command-catalog-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-catalog 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-catalog 'right))
   KeyEvent/VK_UP     (fn [_] (move-catalog 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-catalog 'down))
   KeyEvent/VK_H      (fn [_] (move-catalog 'left))
   KeyEvent/VK_L      (fn [_] (move-catalog 'right))
   KeyEvent/VK_K      (fn [_] (move-catalog 'up))
   KeyEvent/VK_J      (fn [_] (move-catalog 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))

   KeyEvent/VK_ENTER
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

   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   })

(def key-command
  {'cursor  key-command-cursor-mode
   'add     key-command-add-mode
   'move    key-command-move-mode
   'wire    key-command-wire-mode
   'catalog key-command-catalog-mode
   })

;--------------------------------------------------
; key listener for schematic panel
;--------------------------------------------------

(defn make-key-lis [frame panel text-area]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [f ((key-command (:mode @mode)) (.getKeyCode e))]
        (when f (f {:frame frame :text-area text-area})))
      (.repaint panel))
    (keyReleased [e])
    (keyTyped [e])))

;--------------------------------------------------
; key bindings for JTextArea
;--------------------------------------------------

(defn make-key-bindings [panel text-area]
  (let [action (proxy [AbstractAction] []
                 (actionPerformed [ae]
                   (dosync
                     (when @selected-name
                       (ref-set lels
                                (assoc @lels @selected-name
                                       (assoc (@lels @selected-name)
                                              :str
                                              (.getText text-area)))))
                     (ref-set selected-name nil))
                   (.setText text-area "")
                   (.repaint panel)
                   (.requestFocus panel)))]
    (.put (.getInputMap text-area)
          (KeyStroke/getKeyStroke "ENTER")
          "update")
    (.put (.getActionMap text-area)
          "update"
          action)))

;--------------------------------------------------
; main
;--------------------------------------------------

(defn -main []
  (let [frame (JFrame. "catalog")
        content-pane (.getContentPane frame)
        panel (make-panel)
        text-area (JTextArea. 1 40)
        key-lis (make-key-lis frame panel text-area)]
    (make-key-bindings panel text-area)
    (.setFont text-area (Font. Font/MONOSPACED Font/PLAIN 12))
    (.setFocusable panel true)
    (.addKeyListener panel key-lis)
    (.add content-pane panel BorderLayout/CENTER)
    (.add content-pane text-area BorderLayout/SOUTH)
    (.pack frame)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible frame true)
    'done))
