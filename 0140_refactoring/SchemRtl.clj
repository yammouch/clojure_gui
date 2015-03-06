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
  '(javafx.scene.layout  BorderPane Pane VBox)
  '(javafx.scene.paint   Color)
  '(javafx.scene.shape   Rectangle Polygon Polyline Ellipse Line Circle
                         Path PathElement MoveTo ArcTo ClosePath
                         LineTo)
  '(javafx.scene.text    Font Text TextAlignment)
  '(javafx.scene.control Label Button MenuBar Menu MenuItem)
  '(javafx.stage         Stage FileChooser FileChooser$ExtensionFilter))

(require 'clojure.java.io)
(require 'clojure.pprint)
(require 'SchemDialog)
(alias 'sd 'SchemDialog)
(require 'LogicElements)
(alias 'lel 'LogicElements)

(def pix-per-grid 8.0)
(defn grid2screen [[grid-x grid-y]]
  [(* pix-per-grid grid-x)
   (* pix-per-grid grid-y)])

;--------------------------------------------------
; state
;--------------------------------------------------

(def *label-debug* (Label.))

(def cursor-pos (ref {:x 5 :y 5}))
(def cursor-speed (ref 0))

(def selected-lels (ref #{}))
(def selected-wires (ref {}))
(def selected-name (ref nil))

(def moving-lels (ref {}))
(def moving-wires (ref {}))
(def moving-vertices (ref {}))

(def old-lels (ref {}))
(def old-wires (ref {}))

(def mode (ref {:mode :cursor}))
(def wire-p0 (ref {:x 0 :y 0}))
(def catalog-pos (ref {:x 0 :y 0}))

(def lels
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               '[{:type :name , :x 5 , :y 20,
                  :string "hoge", :v-align :bottom, :h-align :left}
                 {:type :in   , :x 25, :y 28, :direction :right}
                 {:type :in   , :x 25, :y 22, :direction :right}
                 {:type :and  , :x 32, :y 28, :direction :right,
                  :width 4, :height 4}
                 {:type :or   , :x 40, :y 23, :direction :right,
                  :width 4, :height 4}
                 {:type :in   , :x 25, :y 30, :direction :right}
                 {:type :in   , :x 25, :y 36, :direction :right}
                 {:type :out  , :x 62, :y 26, :direction :right}
                 {:type :in   , :x 25, :y 34, :direction :right}
                 {:type :and  , :x 32, :y 22, :direction :right,
                  :width 4, :height 4}
                 {:type :dot  , :x 30, :y 29}
                 {:type :dff  , :x 55, :y 26}
                 {:type :mux21, :x 48, :y 24, :direction :right,
                  :width 2, :height 6, :order01 :1->0}
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
;(defn rotate [[x y] degree] ; clockwise, because y gets larger downward
;  (case degree
;    0   [   x     y ]
;    90  [(- y)    x ]
;    180 [(- x) (- y)]
;    270 [   y  (- x)]))
(defn rotate-ofs [[x y] width height degree]
  (case degree
    (  0  :right :horizontal) [             x                y  ]
    ( 90  :down  :vertical  ) [(+ height (- y))              x  ]
    (180  :left             ) [(+ width  (- x)) (+ height (- y))]
    (270  :up               ) [             y   (+ width  (- x))]
    ))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-text [pos str color v-align h-align]
  (let [text (Text. str)]
    (doto text
      (.setFont (Font. "monospaced Regular" 12.0))
      (.setY (+ (* (:y pos) pix-per-grid)
                (case v-align :bottom -2.0, :center 0.0, :top 2.0)))
      (.setTextOrigin (case v-align
                        :bottom VPos/BOTTOM
                        :center VPos/CENTER
                        :top    VPos/TOP))
      (.setStroke color))
    (let [width (.. text getLayoutBounds getWidth)]
      (.setX text (+ (* (:x pos) pix-per-grid)
                     (case h-align
                       :left   (* 0.5 pix-per-grid)
                       :center (- (* 0.5 width))
                       :right  (- (* 0.5 pix-per-grid) width)
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
  (if (= selected '#{p0 p1})
    (let [line (apply #(Line. %1 %2 %3 %4)
                (mapcat grid2screen [[x0 y0] [x1 y1]]))]
      (.setStroke line Color/RED)
      [line])
    (let [x- (- x1 x0) y- (- y1 y0)
          len (Math/sqrt (+ (* x- x-) (* y- y-)))

          [xorg xhl yorg yhl] ; hl: highlight
          (if (= selected #{'p0})
            [x0 (+ x0 (/ x- len)) y0 (+ y0 (/ y- len))]
            [x1 (- x1 (/ x- len)) y1 (- y1 (/ y- len))])
          shortline (apply #(Line. %1 %2 %3 %4)
                     (mapcat grid2screen [[xorg yorg] [xhl yhl]]))
          longline (apply #(Line. %1 %2 %3 %4)
                    (mapcat grid2screen
                            [[xhl yhl] (if (= selected #{'p0})
                                         [x1 y1] [x0 y0])]))]
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
(defmulti lel-draw (fn [lel color & xs] (:type lel)))

(defn unidirectional-port-symbol [lel color]
  (let [symbol (Polygon. (double-array (apply concat
                (map #(grid2screen
                       (map + (rotate-ofs % 3 2 (lel :direction))
                              [(:x lel) (:y lel)]))
                     [[0 0] [2 0] [3 1] [2 2] [0 2]]
                     ))))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (:x lel) (* 0.5 (lel/width  lel)))
                 :y (+ (:y lel) (* 0.5 (lel/height lel)))}
                (case (:type lel) :in "I", :out "O")
                color :center :center)
     symbol]))

; for "in"
(defmethod lel-draw :in [lel color] (unidirectional-port-symbol lel color))

; for "out"
(defmethod lel-draw :out [lel color] (unidirectional-port-symbol lel color))

; for "inout"
(defmethod lel-draw :inout [lel color]
  (let [symbol (Polygon. (double-array
                (mapcat #(grid2screen (map + (rotate-ofs % 3 2 (lel :direction))
                                             [(:x lel) (:y lel)]))
                        [[0 1] [1 0] [2 0] [3 1] [2 2] [1 2]]
                        )))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (:x lel) (* 0.5 (lel/width  lel)))
                 :y (+ (:y lel) (* 0.5 (lel/height lel)))}
                "IO" color :center :center)
     symbol]))

; for "dot"
(defmethod lel-draw :dot [lel color] [(draw-dot lel 7 color)])

; for "name"
(defmethod lel-draw :name [lel color]
  (let [[x y] (grid2screen [(:x lel) (:y lel)])
        line-h (Line. (- x 1.0) y (+ x 1.0) y)
        line-v (Line. x (- y 1.0) x (+ y 1.0))]
    (.setStroke line-h color) (.setStroke line-v color)
    [(draw-text lel (lel :string) color
                (lel :v-align) (lel :h-align))
     line-h line-v]))

; for "not"
(defmethod lel-draw :not [lel color]
  (let [[[x0 y0] [x1 y1] [x2 y2] [cx cy]]
          (map #(grid2screen (map + (rotate-ofs % 3 4 (lel :direction))
                                    [(:x lel) (:y lel)]))
               [[0 0] [2 2] [0 4] [2.5 2.0]])
        triangle (Polygon. (double-array [x0 y0 x1 y1 x2 y2]))
        circle (Circle. cx cy (* 0.5 pix-per-grid))]
    (doto triangle (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto circle   (.setStroke color) (.setFill Color/TRANSPARENT))
    [triangle circle]))

; for "and"
(defmethod lel-draw :and [lel color]
  (let [w (lel :width) h (lel :height)
        [[x0 y0] [x1 y1] [x2 y2] [x3 y3]]
          (map #(grid2screen
                 (map + (rotate-ofs % w h (lel :direction))
                        [(:x lel) (:y lel)]))
               [[0 0] [(* 0.5 w) 0] [(* 0.5 w) h] [0 h]])
        [rx ry] (map #(* 0.5 pix-per-grid %) 
                     (if ('#{:up :down} (lel :direction)) [h w] [w h]))
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
(defmethod lel-draw :or [lel color]
  (let [w (lel :width) h (lel :height)
        [[x0 y0] [x1 y1]]
          (map #(grid2screen
                 (map + (rotate-ofs % w h (lel :direction))
                        [(:x lel) (:y lel)]))
               [[0 0] [0 h]])
        [rx0 ry0 rx1 ry1]
          (map #(* pix-per-grid %)
           (if ('#{:right :left} (lel :direction))
             [w         (* 0.5 h) (* 0.25 w) (* 0.5  h)]
             [(* 0.5 h) w         (* 0.5  h) (* 0.25 w)]))
        symbol (Path. (into-array PathElement
                [(MoveTo. x0 y0)
                 (ArcTo. rx0 ry0 0.0 x1 y1 false true)
                 (ArcTo. rx1 ry1 0.0 x0 y0 false false)
                 (ClosePath.)
                 ]))]
    (doto symbol (.setStroke color) (.setFill Color/TRANSPARENT))
    [symbol]))

; for "dff"
(defmethod lel-draw   :dff [lel color]
  (let [[x y] (grid2screen [(:x lel) (:y lel)])
        rect (Rectangle. x y (* 4 pix-per-grid) (* 5 pix-per-grid))
        line (Polyline. (double-array
              (mapcat #(grid2screen (map + % [(:x lel) (:y lel)]))
                      [[1 5] [2 4] [3 5]])))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    (doto line (.setStroke color))
    [rect line]))

; for "dffr"
(defmethod lel-draw   :dffr [lel color]
  (conj (lel-draw (assoc lel :type :dff) color)
        (draw-text {:x (+ (:x lel) 2) :y (:y lel)}
                   "R" color :top :center
                   )))

; for "mux21"
(defmethod lel-draw :mux21 [lel color]
  (concat
   (lel-draw (assoc lel :type :mux-n) color)
   (let [w (lel :width) h (lel :height)
         [[ax ay] [bx by]] (map #(map + (rotate-ofs % w h (lel :direction))
                                        [(:x lel) (:y lel)])
                                [[(* 0.5 w) 2] [(* 0.5 w) (- h 2)]]
                                )]
     [(draw-text {:x ax :y ay} (if (= (lel :order01) :0->1) "0" "1")
                 color :center :center)
      (draw-text {:x bx :y by} (if (= (lel :order01) :0->1) "1" "0")
                 color :center :center
                 )])))

; for "mux-n"
(defmethod lel-draw :mux-n [lel color]
  (let [h (lel :height) w (lel :width)
        trapezoid (Polygon. (double-array
                   (mapcat #(grid2screen
                             (map + (rotate-ofs % w h (lel :direction))
                                    [(:x lel) (:y lel)]))
                           [[0 0] [w 2] [w (- h 2)] [0 h]]
                           )))]
    (doto trapezoid (.setStroke color) (.setFill Color/TRANSPARENT))
    [trapezoid]))

; for "plus" and "minus"
(defn arith-symbol [lel color]
  (let [[x y] (grid2screen [(:x lel) (:y lel)])
        rect (Rectangle. x y (* 4 pix-per-grid) (* 4 pix-per-grid))]
    (doto rect (.setStroke color) (.setFill Color/TRANSPARENT))
    [(draw-text {:x (+ (lel :x) (* 0.5 (lel/width  lel)))
                 :y (+ (lel :y) (* 0.5 (lel/height lel)))}
                (case (:type lel) :plus "+", :minus "-")
                color :center :center)
     rect]))

; for "plus"
(defmethod lel-draw   :plus [lel color] (arith-symbol lel color))

; for "minus"
(defmethod lel-draw   :minus [lel color] (arith-symbol lel color))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor [cursor-pos lels wires]
  (into-array Node
   (concat
    [(draw-dot cursor-pos 9 Color/BLUE)]
    (mapcat (fn [[k v]]
              (let [selected (@selected-wires k)]
                (if selected
                  (draw-wire-selected v selected)
                  [(draw-wire v Color/BLACK)])))
            wires)
    (mapcat (fn [[k v]]
              (lel-draw v (if (@selected-lels k) Color/RED Color/BLACK)))
            lels)
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
                 (into-array Double [2.0 2.0]))
        [rect])))))

(defn draw-mode-move []
  (into-array Node
   (concat
    [(draw-dot @cursor-pos 9 Color/BLUE)]
    (mapcat (fn [[k v]]
              (let [vertices (@moving-vertices k)]
                (if vertices
                  (draw-wire-selected v vertices)
                  [(draw-wire v Color/BLACK)])))
            @wires)
    (mapcat (fn [[_ v]] (lel-draw v Color/BLACK)) @lels)
    (map (fn [[_ v]] (draw-wire v Color/RED)) @moving-wires)
    (mapcat (fn [[_ v]] (lel-draw v Color/RED)) @moving-lels)
    )))

(defn draw-mode-add []
  (into-array Node
   (concat
    (map (fn [[k v]] (draw-wire v Color/BLACK)) @wires)
    (mapcat (fn [[k v]] (lel-draw v Color/BLACK)) @lels)
    (lel-draw (conj (lel/lel-init (:type @mode))
                    @cursor-pos)
              Color/RED))))

(defn draw-mode-wire []
  (into-array Node
   (concat
    [(draw-dot @cursor-pos 9 Color/BLUE)]
    (map (fn [[k v]] (draw-wire v Color/BLACK)) @wires)
    (mapcat (fn [[k v]] (lel-draw v Color/BLACK)) @lels)
    [(draw-wire {:x0 (@wire-p0 :x) :y0 (@wire-p0 :y)
                 :x1 (@cursor-pos :x) :y1 (@cursor-pos :y)}
                Color/RED)])))

(def catalog-table
  [[:in    :out   :inout :dot   :name]
   [:not   :and   :or    :dff   :dffr]
   [:mux21 :mux-n :plus  :minus      ]])

(defn draw-mode-catalog []
  (let [parts (mapcat (fn [idx0 parts]
                        (map (fn [idx1 part]
                               {:idx0 idx0, :idx1 idx1, :part part})
                             (range) parts))
                      (range) catalog-table)
        rect (Rectangle. (* pix-per-grid (+ (* 10 (@catalog-pos :x)) 1))
                         (* pix-per-grid (+ (* 10 (@catalog-pos :y)) 1))
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

(defn draw-mode []
  (case (@mode :mode)
    :cursor  (draw-mode-cursor @cursor-pos @lels @wires)
    :move    (draw-mode-move)
    :add     (draw-mode-add)
    :wire    (draw-mode-wire)
    :catalog (draw-mode-catalog)
    ))

;--------------------------------------------------
; move-*
;--------------------------------------------------

(defn move-cursor [dir speed]
  (dosync (alter cursor-pos #(update-in % [dir] (partial + speed)))))

(defn move-selected [dir speed]
  (dosync
    (alter moving-lels lel/move-lels dir speed)
    (alter moving-wires lel/move-wires dir speed)
    (alter wires lel/move-wires-by-vertices @moving-vertices dir speed)))

(defn move-catalog [dir speed]
  (dosync (alter catalog-pos
           #(update-in % [dir] (if (neg? speed) dec inc))
           )))

;--------------------------------------------------
; sub functions for key commands
;--------------------------------------------------

(defn release-selection []
  (dosync
    (ref-set selected-lels #{})
    (ref-set selected-wires {})))

;;(defn close-window [frame]
;;  (let [yn (JOptionPane/showConfirmDialog
;;            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
;;    (when (= yn JOptionPane/YES_OPTION)
;;      (.dispose frame))))
;;

;--------------------------------------------------
; dialog box
;--------------------------------------------------

(defn dialog-table [type]
  (case type
    (:in :out) [[:radio :direction :right :up :left :down]]
    :inout     [[:radio :direction :horizontal :vertical]]
    :inv       [[:radio :direction :right :up :left :down]]
    (:and :or :mux-n)
               [[:edstr :width  read-string]
                [:edstr :height read-string]
                [:radio :direction :right :up :left :down]]
    :not       [[:radio :direction :right :up :left :down]]
    :name      [[:edstr :string identity]
                [:radio :h-align :left   :center :right]
                [:radio :v-align :bottom :center :top  ]]
    :mux21     [[:edstr :width  read-string]
                [:edstr :height read-string]
                [:radio :direction :right :up :left :down]
                [:radio :order01 :0->1 :1->0]]
    nil))

;--------------------------------------------------
; key commands for each mode on schematic panel
;--------------------------------------------------

(def key-command-cursor-mode
  {;KeyCode/VK_Q      (fn [{frame :frame}] (close-window frame))
   ; cursor -> catalog
   KeyCode/C      (fn [_] (dosync (ref-set mode {:mode :catalog})))
   ; cursor -> move
   KeyCode/M
   (fn [_]
     (dosync
       (ref-set old-lels @lels)
       (ref-set old-wires @wires)
       (let [{:keys [s n]} (group-by #(if (selected-lels (% 0)) :s :n)
                                     @lels)]
         (ref-set moving-lels (into {} s))
         (ref-set        lels (into {} n)))
       (let [{:keys [s n]} (group-by #(if (= (selected-wires (% 0)) '#{p0 p1})
                                        :s :n)
                                     @wires)]
         (ref-set moving-wires (into {} s))
         (ref-set        wires (into {} n)))
       (ref-set moving-vertices
                (into {} (filter (fn [[_ v]] (not= '#{p0 p1} v))
                                 @selected-wires)))
       (ref-set mode {:mode :move})
       (release-selection)))
   ; cursor -> wire
   KeyCode/W      (fn [_] (dosync
                            (release-selection)
                            (ref-set wire-p0 @cursor-pos)
                            (ref-set mode {:mode :wire})))
   ; no mode change
   KeyCode/R
   (fn [_]
     (if (:rect-x0 @mode)
       (dosync (alter mode dissoc :rect-x0 :rect-y0))
       (dosync (alter mode conj {:rect-x0 (@cursor-pos :x)
                                 :rect-y0 (@cursor-pos :y)}))))
   KeyCode/ENTER
   (fn [_]
     (let [lel-key (lel/find-lel-by-pos @lels @cursor-pos)
           wire-key (lel/find-wires-by-pos @wires @cursor-pos)
           rect-keys (if (@mode :rect-x0)
                       (lel/rectangular-select @lels @wires
                         (@mode :rect-x0) (@mode :rect-y0)
                         (@cursor-pos :x) (@cursor-pos :y))
                       {})]
       (dosync
         (when lel-key
           (alter selected-lels conj lel-key))
         (when (:lels rect-keys)
           (alter selected-lels into (:lels rect-keys)))
         (when wire-key
           (alter selected-wires lel/merge-selected-wire wire-key))
         (when (:wires rect-keys)
           (alter selected-wires lel/merge-selected-wire (:wires rect-keys)))
         (alter mode dissoc :rect-x0 :rect-y0)
         )))
   KeyCode/ESCAPE (fn [_]
                    (dosync (alter mode dissoc :rect-x0 :rect-y0))
                    (release-selection))
   KeyCode/X      (fn [_] (dosync
                            (alter lels lel/remove-lel-by-key @selected-lels)
                            (alter wires lel/remove-wire-by-key @selected-wires)
                            (ref-set selected-lels #{})
                            ))})

; add mode can be merged into move mode
; if continuous addition is not necessary.
(def key-command-add-mode
  {;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   ; add -> catalog
   KeyCode/C      (fn [_] (dosync (ref-set mode {:mode :catalog})))
   ; add -> cursor
   KeyCode/ESCAPE (fn [_] (dosync (ref-set mode {:mode :cursor})))
   ; no mode change
   KeyCode/ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym)
               (conj (lel/lel-init (:type @mode))
                     @cursor-pos)})))
   })

(def key-command-move-mode
  {;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   ; move -> cursor
   KeyCode/ESCAPE (fn [_] (dosync
                            (ref-set lels @old-lels)
                            (ref-set wires @old-wires)
                            (ref-set mode {:mode :cursor})))
   KeyCode/ENTER  (fn [_] (dosync
                            (alter lels conj @moving-lels)
                            (alter wires conj @moving-wires)
                            (ref-set mode {:mode :cursor})))
   })

(def key-command-wire-mode
  {;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   ; wire -> cursor
   KeyCode/ESCAPE (fn [_] (dosync
                            (ref-set mode {:mode :cursor})))
   KeyCode/ENTER  (fn [_] (dosync
                            (alter wires conj
                                   {(gensym) {:x0 (@wire-p0 :x)
                                              :y0 (@wire-p0 :y)
                                              :x1 (@cursor-pos :x)
                                              :y1 (@cursor-pos :y)}})
                            (ref-set mode {:mode :cursor})))
                            })

(def key-command-catalog-mode
  {;KeyCode/Q      (fn [{frame :frame}] (close-window frame))
   ; catalog -> add
   KeyCode/ENTER
   (fn [_]
     (when-let [type (get-in catalog-table (map @catalog-pos [:y :x]))]
       (dosync (ref-set mode {:mode :add :type type}))))
   ; catalog -> cursor
   KeyCode/ESCAPE (fn [_] (dosync (ref-set mode {:mode :cursor})))
   })

(def key-command
  {:cursor  key-command-cursor-mode
   :add     key-command-add-mode
   :move    key-command-move-mode
   :wire    key-command-wire-mode
   :catalog key-command-catalog-mode
   })

(defn state-text []
  (reduce #(str %1 "\n" %2)
          [@mode @wire-p0 @cursor-pos @cursor-speed
           @catalog-pos @lels @selected-lels
           @wires @selected-wires @selected-name]))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(defn pane-schem-cursor-speed [keyEvent]
  (let [num ({KeyCode/DIGIT0 0, KeyCode/DIGIT1 1, KeyCode/DIGIT2 2,
              KeyCode/DIGIT3 3, KeyCode/DIGIT4 4, KeyCode/DIGIT5 5,
              KeyCode/DIGIT6 6, KeyCode/DIGIT7 7, KeyCode/DIGIT8 8,
              KeyCode/DIGIT9 9, KeyCode/MINUS :-}
             (.getCode keyEvent))]
    (when (and num (#{:cursor :add :wire :move} (:mode @mode)))
      (dosync (ref-set cursor-speed
               (if (= num :-) 0 (+ (* @cursor-speed 10) num))))
      (.setText *label-debug* (state-text))
      true)))

(defn pane-schem-cursor-move [keyEvent pane]
  (let [kc (.getCode keyEvent)
        dir (cond (#{KeyCode/LEFT  KeyCode/H} kc) :left
                  (#{KeyCode/RIGHT KeyCode/L} kc) :right
                  (#{KeyCode/UP    KeyCode/K} kc) :up
                  (#{KeyCode/DOWN  KeyCode/J} kc) :down
                  :else                           nil)
        speed (cond (not dir)            1
                    (<= @cursor-speed 0)
                      (lel/jump-amount dir @cursor-pos @lels @wires)
                    ('#{:left :up} dir)  (- @cursor-speed)
                    :else                @cursor-speed)
        op (case (:mode @mode)
             (:cursor :add :wire) #(move-cursor % speed)
             :move                #(do (move-cursor   % speed)
                                       (move-selected % speed))
             :catalog             #(move-catalog % (case dir (:left :up) -1 1))
             nil)]
    (when (and dir op)
      (op (case dir (:left :right) :x :y))
      (.consume keyEvent)
      (.setText *label-debug* (state-text))
      (.setAll (.getChildren pane) (draw-mode))
      true)))

(defn pane-schem-revert [f-set-to-parent pane]
  (.setText *label-debug* (state-text))
  (f-set-to-parent pane)
  (.setAll (.getChildren pane) (draw-mode))
  (.setFocusTraversable pane true)
  (.requestFocus pane))

(defn pane-schem-goto-dialog [keyEvent pane f-set-to-parent]
  (when (and (= KeyCode/D (.getCode keyEvent))
             (= (:mode @mode) :cursor))
    (let [lel-key (lel/find-lel-by-pos @lels @cursor-pos)]
      (when-let [dt (dialog-table (get-in @lels [lel-key :type]))]
        (.setFocusTraversable pane false)
        (dosync (ref-set selected-name lel-key))
        (let [borderpane (BorderPane.)
              dialog
                (sd/pane-dialog #(.setRight borderpane %)
                 #(pane-schem-revert f-set-to-parent pane)
                 dt (@lels lel-key)
                 #(dosync (alter lels assoc lel-key %))
                 )]
          (.setCenter borderpane pane)
          (f-set-to-parent borderpane)
          (.setFocusTraversable dialog true)
          (.requestFocus dialog)
          true)))))

(defn pane-schem-key [f-set-to-parent pane]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (or (pane-schem-goto-dialog  keyEvent pane f-set-to-parent)
          (pane-schem-cursor-move  keyEvent pane)
          (pane-schem-cursor-speed keyEvent)
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
       [(FileChooser$ExtensionFilter. "RTL Schematica (*.rtc)"
         (into-array String ["*.rtc"] ))
        (FileChooser$ExtensionFilter. "All Files (*.*)"
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

