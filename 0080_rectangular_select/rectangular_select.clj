(ns rectangular-select
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font BasicStroke BorderLayout])
(import '[java.awt.font TextLayout])
(import '[javax.swing JFrame JPanel JOptionPane JTextArea KeyStroke
                      AbstractAction])
(import '[java.awt.event KeyListener KeyEvent])

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
; subfunctions for draw-*
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

;--------------------------------------------------
; draw-* functions to draw parts
;--------------------------------------------------

(defn draw-in [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 2 3 2 0]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 0 1 2 2]))
                5)
  (draw-text g {:x (+ (pos :x) 1.5) :y (+ (pos :y) 1)}
             "I" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

(defn draw-out [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 2 3 2 0]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 0 1 2 2]))
                5)
  (draw-text g {:x (+ (pos :x) 1.5) :y (+ (pos :y) 1)}
             "O" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

(defn draw-inout [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 1 2 3 2 1]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [1 0 0 1 2 2]))
                6)
  (draw-text g {:x (+ (pos :x) 1.5) :y (+ (pos :y) 1)}
             "IO" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

(defn draw-dot [g pos size color]
  (let [x (- (* (pos :x) pix-per-grid)
             (int (* 0.5 size)))
        y (- (* (pos :y) pix-per-grid)
             (int (* 0.5 size)))]
    (.setColor g color)
    (.fillOval g x y size size)))

(defn draw-name [g lel color]
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

(defn draw-not [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 2 0]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 2 4]))
                3)
  (.drawOval g
             (int (* (+ (pos :x) 2  ) pix-per-grid))
             (int (* (+ (pos :y) 1.5) pix-per-grid))
             pix-per-grid
             pix-per-grid))

(defn draw-and [g pos color]
  (.setColor g color)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                 [2 0 0 2]))
                 (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                 [0 0 4 4]))
                 4)
  (.drawArc g
            (int (* (pos :x) pix-per-grid))
            (int (* (pos :y) pix-per-grid))
            (* 4 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180))

(defn draw-or [g pos color]
  (.setColor g color)
  (.drawArc g
            (int (* (- (pos :x) 1) pix-per-grid))
            (int (* (pos :y) pix-per-grid))
            (* 2 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180)
  (.drawArc g
            (int (* (- (pos :x) 4) pix-per-grid))
            (int (* (pos :y) pix-per-grid))
            (* 8 pix-per-grid)
            (* 4 pix-per-grid)
            -90
            180))

(let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
  (defn draw-status [g objs]
    (.setColor g Color/BLUE)
    (.setFont g font)
    (doseq [[obj ypos] (map #(list (if (nil? %1) "nil" %1)
                                   (+ 12 (* 12 %2)))
                            objs (range))]
      (.drawString g (.toString obj) 2 ypos))))

(defn draw-mux21 [g pos color]
  (let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
    (draw-text g {:x (+ (pos :x) 1) :y (+ (pos :y) 2)}
               "0" color font 'm 'c)
    (draw-text g {:x (+ (pos :x) 1) :y (+ (pos :y) 4)}
               "1" color font 'm 'c)
    (.setColor g color)
    (.drawPolygon g
                  (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                  [0 2 2 0]))
                  (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                  [0 2 4 6]))
                  4)))

(defn draw-dff [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 0 4 4]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 5 5 0]))
                4)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                 [1 2 3]))
                 (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                 [5 4 5]))
                 3))

(defn draw-plus [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 4 4 0]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 0 4 4]))
                4)
  (draw-text g {:x (+ (pos :x) 2) :y (+ (pos :y) 2)}
             "+" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

(defn draw-minus [g pos color]
  (.setColor g color)
  (.drawPolygon g
                (int-array (map #(* pix-per-grid (+ (pos :x) %))
                                [0 4 4 0]))
                (int-array (map #(* pix-per-grid (+ (pos :y) %))
                                [0 0 4 4]))
                4)
  (draw-text g {:x (+ (pos :x) 2) :y (+ (pos :y) 2)}
             "-" color (Font. Font/MONOSPACED Font/PLAIN 12) 'm 'c))

(defn draw-wire [g {x0 :x0 y0 :y0 x1 :x1 y1 :y1} color]
  (.setColor g color)
  (.drawPolyline g
                 (int-array (map #(* pix-per-grid %) [x0 x1]))
                 (int-array (map #(* pix-per-grid %) [y0 y1]))
                 2))

(defn draw-lel [g lel color]
  (case (lel :type)
    in    (draw-in    g lel color)
    out   (draw-out   g lel color)
    inout (draw-inout g lel color)
    dot   (draw-dot   g lel 7 color)
    name  (draw-name  g lel color)
    not   (draw-not   g lel color)
    and   (draw-and   g lel color)
    or    (draw-or    g lel color)
    dff   (draw-dff   g lel color)
    mux21 (draw-mux21 g lel color)
    plus  (draw-plus  g lel color)
    minus (draw-minus g lel color)))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor [g]
  (draw-dot g @cursor-pos 9 Color/BLUE)
  (doseq [[k v] @wires]
    (draw-wire g v (if (@selected-wires k) Color/RED Color/BLACK)))
  (doseq [[k v] @lels]
    (draw-lel g v (if (@selected-lels k) Color/RED Color/BLACK)))
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
    (draw-lel g v Color/BLACK))
  (draw-lel g
            (conj {:type (:type @mode)} @cursor-pos)
            Color/RED))

(defn draw-mode-wire [g]
  (draw-dot g @cursor-pos 9 Color/BLUE)
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (draw-lel g v Color/BLACK))
  (draw-wire g
             {:x0 (@wire-p0 :x) :y0 (@wire-p0 :y)
              :x1 (@cursor-pos :x) :y1 (@cursor-pos :y)}
             Color/RED))

(def catalog-table
  [[{:name 'in    :w 3 :h 2
     :fdraw (fn [g pos] (draw-in g pos Color/BLACK))}
    {:name 'out   :w 3 :h 2
     :fdraw (fn [g pos] (draw-out g pos Color/BLACK))}
    {:name 'inout :w 3 :h 2
     :fdraw (fn [g pos] (draw-inout g pos Color/BLACK))}
    {:name 'dot   :w 2 :h 2
     :fdraw (fn [g pos] (draw-dot g pos 7 Color/BLACK))}
    {:name 'name  :w 2 :h 4
     ;:fdraw (fn [g pos] (draw-name g pos Color/BLACK))}
     :fdraw (fn [g pos] (draw-text g pos "blah" Color/BLACK
                                   (Font. Font/MONOSPACED Font/PLAIN 12)
                                   't 'l))}]
   [{:name 'not   :w 4 :h 4
     :fdraw (fn [g pos] (draw-not g pos Color/BLACK))}
    {:name 'and   :w 4 :h 4
     :fdraw (fn [g pos] (draw-and g pos Color/BLACK))}
    {:name 'or    :w 4 :h 4
     :fdraw (fn [g pos] (draw-or g pos Color/BLACK))}
    {:name 'dff   :w 4 :h 5
     :fdraw (fn [g pos] (draw-dff g pos Color/BLACK))}
    {:name 'mux21 :w 2 :h 6
     :fdraw (fn [g pos] (draw-mux21 g pos Color/BLACK))}
     ]
   [{:name 'plus  :w 4 :h 4
     :fdraw (fn [g pos] (draw-plus g pos Color/BLACK))}
    {:name 'minus :w 4 :h 4
     :fdraw (fn [g pos] (draw-minus g pos Color/BLACK))}
     ]])

(defn draw-mode-catalog [g]
  (doseq [[idx0 parts] (map #(list %1 %2) (range) catalog-table)]
    (doseq [[idx1 part] (map #(list %1 %2) (range) parts)]
      ((part :fdraw) g
                     {:x (- (+ (* 10 idx1) 6)
                            (int (/ (part :w) 2)))
                      :y (- (+ (* 10 idx0) 6)
                            (int (/ (part :h) 2))
                            )})))
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
  (let [xmin (Math/min x0 x1)
        xmax (Math/max x0 x1)
        ymin (Math/min y0 y1)
        ymax (Math/max y0 y1)
        lels (filter (fn [[k v]] (<= xmin (:x v)))
                     lels)
        lels (filter (fn [[k v]] (<= ymin (:y v)))
                     lels)
        lels (filter (fn [[k v]]
                       (<= (+ (:x v)
                              (case (:type v)
                                in 3, out 3, inout 3, dot 0, name 0,
                                and 4, or 4, dff 4, mux21 2,
                                plus 4, minus 4))
                           xmax)) ; symbol size should be returned
                     lels)        ; from a function like 'symbol-size'
        lels (filter (fn [[k v]]
                       (<= (+ (:y v)
                              (case (:type v)
                                in 2, out 2, inout 2, dot 0, name 0,
                                and 4, or 4, dff 5, mux21 6,
                                plus 4, minus 4))
                           ymax))
                     lels)
        wires (filter (fn [[k v]]
                        (<= xmin (Math/min (:x0 v) (:x1 v))))
                      wires)
        wires (filter (fn [[k v]]
                        (<= ymin (Math/min (:y0 v) (:y1 v))))
                      wires)
        wires (filter (fn [[k v]]
                        (<= xmax (Math/max (:x0 v) (:x1 v))))
                      wires)
        wires (filter (fn [[k v]]
                        (<= ymax (Math/max (:y0 v) (:y1 v))))
                      wires)]
    {:lels (set (keys lels)) :wires (set (keys wires))}
    ))

(def catalog-table
  [[{:name 'in    :w 3 :h 2
     :fdraw (fn [g pos] (draw-in g pos Color/BLACK))}
    {:name 'out   :w 3 :h 2
     :fdraw (fn [g pos] (draw-out g pos Color/BLACK))}
    {:name 'inout :w 3 :h 2
     :fdraw (fn [g pos] (draw-inout g pos Color/BLACK))}
    {:name 'dot   :w 2 :h 2
     :fdraw (fn [g pos] (draw-dot g pos 7 Color/BLACK))}
    {:name 'name  :w 2 :h 4
     ;:fdraw (fn [g pos] (draw-name g pos Color/BLACK))}
     :fdraw (fn [g pos] (draw-text g pos "blah" Color/BLACK
                                   (Font. Font/MONOSPACED Font/PLAIN 12)
                                   't 'l))}]
   [{:name 'not   :w 4 :h 4
     :fdraw (fn [g pos] (draw-not g pos Color/BLACK))}
    {:name 'and   :w 4 :h 4
     :fdraw (fn [g pos] (draw-and g pos Color/BLACK))}
    {:name 'or    :w 4 :h 4
     :fdraw (fn [g pos] (draw-or g pos Color/BLACK))}
    {:name 'dff   :w 4 :h 5
     :fdraw (fn [g pos] (draw-dff g pos Color/BLACK))}
    {:name 'mux21 :w 2 :h 6
     :fdraw (fn [g pos] (draw-mux21 g pos Color/BLACK))}
     ]
   [{:name 'plus  :w 4 :h 4
     :fdraw (fn [g pos] (draw-plus g pos Color/BLACK))}
    {:name 'minus :w 4 :h 4
     :fdraw (fn [g pos] (draw-minus g pos Color/BLACK))}
     ]])


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
                      (find-wire-by-pos @wires @cursor-pos))]
       (dosync
         (when lel-key
           (alter selected-lels conj lel-key))
         (when wire-key
           (alter selected-wires conj wire-key)
           ))))
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
              {(gensym) (conj @cursor-pos {:type (:type @mode)})}
              )))
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
     (dosync
       (ref-set mode
                {:mode 'add
                 :type (:name (try
                                (nth (nth catalog-table
                                          (:y @catalog-pos))
                                     (:x @catalog-pos))
                                (catch IndexOutOfBoundsException e nil)))})))

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
                     (ref-set lels
                              (assoc @lels @selected-name
                                     (assoc (@lels @selected-name)
                                            :str
                                            (.getText text-area))))
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
