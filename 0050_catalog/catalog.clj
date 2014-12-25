(ns catalog
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font BasicStroke])
(import '[java.awt.font TextLayout])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(def pix-per-grid 8)

(defn -init []
  [[] (atom [])])

;--------------------------------------------------
; states
;--------------------------------------------------

(def lels (ref (let [g0 (gensym)
                     g1 (gensym)]
                 {g0 {:type 'dff :x 2 :y 2}
                  g1 {:type 'mux21 :x 10 :y 10}
                  })))
; Clojure 1.6.0 does not accept {(gensym) x (gensym) y}
; by saying (gensym)s are duplicated. Bug?
; It is the same for #{(gensym) (gensym)}.
(def selected-lels (ref #{}))
(def selected-wires (ref #{}))

(def wires
  (let [g0 (gensym)
        g1 (gensym)
        g2 (gensym)]
    (ref {g0 {:x0 10 :y0 10 :x1 20 :y1 10}
          g1 {:x0 20 :y0 10 :x1 20 :y1 20}
          g2 {:x0 20 :y0 10 :x1 30 :y1  5}
          })))

(def cursor-pos (ref {:x 5 :y 5}))
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
    (doseq [[obj ypos] (map #(list %1 (+ 12 (* 12 %2))) objs (range))]
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
    not   (draw-not   g lel color)
    and   (draw-and   g lel color)
    or    (draw-or    g lel color)
    dff   (draw-dff   g lel color)
    mux21 (draw-mux21 g lel color)))

;--------------------------------------------------
; draw-mode-*
;--------------------------------------------------

(defn draw-mode-cursor [g]
  (draw-dot g @cursor-pos 9 Color/BLUE)
  (doseq [[k v] @wires]
    (draw-wire g v (if (@selected-wires k) Color/RED Color/BLACK)))
  (doseq [[k v] @lels]
    (draw-lel g v (if (@selected-lels k) Color/RED Color/BLACK))))

(defn draw-mode-add [g]
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (draw-lel g v Color/BLACK))
  (draw-lel g
            (conj {:type (:type @mode)} @cursor-pos)
            Color/RED))

(defn draw-mode-dff [g]
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (draw-lel g v Color/BLACK))
  (draw-lel g
            (conj {:type 'dff} @cursor-pos)
            Color/RED))

(defn draw-mode-mux21 [g]
  (doseq [[k v] @wires]
    (draw-wire g v Color/BLACK))
  (doseq [[k v] @lels]
    (draw-lel g v Color/BLACK))
  (draw-lel g
            (conj {:type 'mux21} @cursor-pos)
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
      (draw-status g [@cursor-pos @mode @lels @selected-lels
                      @wires @selected-wires @catalog-pos])
      (case (@mode :mode)
        cursor  (draw-mode-cursor  g)
        move    (draw-mode-cursor  g)
        add     (draw-mode-add     g)
        dff     (draw-mode-dff     g)
        mux21   (draw-mode-mux21   g)
        wire    (draw-mode-wire    g)
        catalog (draw-mode-catalog g)))
    (getPreferredSize []
      (Dimension. 800 400))))

;--------------------------------------------------
; move-*
;--------------------------------------------------

(defn move-cursor [dir]
  (dosync
    (ref-set cursor-pos
             (case dir
               left  (assoc @cursor-pos :x (dec (@cursor-pos :x)))
               right (assoc @cursor-pos :x (inc (@cursor-pos :x)))
               up    (assoc @cursor-pos :y (dec (@cursor-pos :y)))
               down  (assoc @cursor-pos :y (inc (@cursor-pos :y)))
               ))))

(defn move-selected-lels [dir]
  (dosync
    (ref-set lels
             (reduce (fn [lels sel]
                       (assoc lels sel
                              (assoc (lels sel)
                                     (cond (#{'left 'right} dir) :x
                                           (#{'up 'down} dir)    :y)
                                     (case dir
                                       left  (dec ((lels sel) :x))
                                       right (inc ((lels sel) :x))
                                       up    (dec ((lels sel) :y))
                                       down  (inc ((lels sel) :y))
                                       ))))
                     @lels
                     @selected-lels))))

(defn move-wire [wire dir]
  (let [[f & keys] (case dir
                     left  [dec :x0 :x1]
                     right [inc :x0 :x1]
                     up    [dec :y0 :y1]
                     down  [inc :y0 :y1])]
    (reduce (fn [wire k] (assoc wire k (f (wire k))))
            wire keys)))

(defn move-selected-wires [dir]
  (let [moved (reduce (fn [wires sel]
                        (assoc wires sel
                               (move-wire (wires sel) dir)))
                      @wires
                      @selected-wires)]
    (dosync
      (ref-set wires moved)
      )))

(defn move-selected [dir]
  (move-selected-lels dir)
  (move-selected-wires dir))

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
; key commands for each mode
;--------------------------------------------------

(def key-command-cursor-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down))
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
   KeyEvent/VK_ESCAPE (fn [_] (release-selection))
   KeyEvent/VK_X      (fn [_] (dosync
                                (alter lels remove-lel-by-key @selected-lels)
                                (alter wires remove-lel-by-key @selected-wires)
                                (ref-set selected-lels #{})
                                ))})

(def key-command-add-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym) (conj @cursor-pos {:type (:type @mode)})}
              )))
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   })

(def key-command-dff-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym) (conj @cursor-pos {:type 'dff})}
              )))
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   KeyEvent/VK_B      (fn [_] (dosync (ref-set mode {:mode 'mux21})))
   })

(def key-command-mux21-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ENTER
   (fn [_]
     (dosync
       (alter lels conj
              {(gensym) (conj @cursor-pos {:type 'mux21})}
              )))
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode {:mode 'cursor})))
   KeyEvent/VK_A      (fn [_] (dosync (ref-set mode {:mode 'dff})))
   })

(def key-command-move-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor   'left)
                              (move-selected 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor   'right)
                              (move-selected 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor   'up)
                              (move-selected 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor   'down)
                              (move-selected 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor   'left)
                              (move-selected 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor   'right)
                              (move-selected 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor   'up)
                              (move-selected 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor   'down)
                              (move-selected 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ESCAPE (fn [_] (dosync
                                (release-selection)
                                (ref-set mode {:mode 'cursor})))
                                })

(def key-command-wire-mode  
  {KeyEvent/VK_LEFT   (fn [_] (move-cursor 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-cursor 'right))
   KeyEvent/VK_UP     (fn [_] (move-cursor 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-cursor 'down))
   KeyEvent/VK_H      (fn [_] (move-cursor 'left))
   KeyEvent/VK_L      (fn [_] (move-cursor 'right))
   KeyEvent/VK_K      (fn [_] (move-cursor 'up))
   KeyEvent/VK_J      (fn [_] (move-cursor 'down))
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
   'dff     key-command-dff-mode
   'mux21   key-command-mux21-mode
   'move    key-command-move-mode
   'wire    key-command-wire-mode
   'catalog key-command-catalog-mode
   })

;--------------------------------------------------
; key listener
;--------------------------------------------------

(defn make-key-lis [frame panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [f ((key-command (:mode @mode)) (.getKeyCode e))]
        (when f (f {:frame frame})))
      (.repaint panel))
    (keyReleased [e])
    (keyTyped [e])))

;--------------------------------------------------
; main
;--------------------------------------------------

(defn -main []
  (let [frame (JFrame. "catalog")
        panel (make-panel)
        key-lis (make-key-lis frame panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener key-lis))
    (doto frame
      (.add panel)
      (.pack)
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    'done))
