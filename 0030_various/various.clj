(ns various
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font])
(import '[java.awt.font TextLayout])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(require 'clojure.set)

(def lels (ref (let [g0 (gensym)
                     g1 (gensym)]
                 {g0 {:type 'dff :x 2 :y 2}
                  g1 {:type 'mux21 :x 10 :y 10}
                  })))
; Clojure 1.6.0 does not accept {(gensym) x (gensym) y}
; by saying (gensym)s are duplicated. Bug?
; It is the same for #{(gensym) (gensym)}.
(def selected (ref #{}))

(def cursor-pos (ref {:x 5 :y 5}))
(def pix-per-grid 8)
(def mode (ref 'cursor))

(defn -init []
  [[] (atom [])])

(defn draw-cursor [g pos]
  (let [cursor-size 10
        x (- (* (pos :x) pix-per-grid)
             (* 0.5 cursor-size))
        y (- (* (pos :y) pix-per-grid)
             (* 0.5 cursor-size))]
    (.setColor g Color/BLUE)
    (.fillOval g x y cursor-size cursor-size)))

(let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
  (defn draw-status [g objs]
    (.setColor g Color/BLUE)
    (.setFont g font)
    (doseq [[obj ypos] (map #(list %1 (+ 12 (* 12 %2))) objs (range))]
      (.drawString g (.toString obj) 2 ypos))))

(defn draw-mux21 [g pos color]
  (let [grid pix-per-grid
        [x y] (map #(* grid (pos %)) [:x :y])
        font (Font. Font/MONOSPACED Font/PLAIN grid)
        frc (.getFontRenderContext g)
        [bound0 bound1] (map #(.getBounds (TextLayout. % font frc))
                             ["0" "1"])
        wh2ofs #(int (+ 0.5 (* 0.5 %)))
        [ofs0-x ofs1-x] (map #(wh2ofs (.getWidth %)) [bound0 bound1])
        [ofs0-y ofs1-y] (map #(wh2ofs (.getHeight %)) [bound0 bound1])]
    (.setColor g color)
    (.drawString g "0"
                 (int (+ x (* grid 1) (- ofs0-x)))
                 (int (+ y (* grid 2) ofs0-y)))
    (.drawString g "1"
                 (int (+ x (* grid 1) (- ofs1-x)))
                 (int (+ y (* grid 4) ofs1-y)))
    (.drawPolygon g
                  (int-array [x (+ x (* 2 grid)) (+ x (* 2 grid)) x])
                  (int-array [y (+ y (* 2 grid)) (+ y (* 4 grid))
                              (+ y (* 6 grid))])
                  4)))

(defn draw-dff [g pos color]
  (let [grid pix-per-grid
        [x y] (map #(* grid (pos %)) [:x :y])]
    (.setColor g color)
    (.drawPolygon g
                  (int-array [x x (+ x (* grid 4)) (+ x (* grid 4))])
                  (int-array [y (+ y (* grid 5)) (+ y (* grid 5)) y])
                  4)
    (.drawPolyline g
                   (int-array [(+ x grid) (+ x (* grid 2))
                               (+ x (* grid 3))])
                   (int-array [(+ y (* grid 5)) (+ y (* grid 4))
                               (+ y (* grid 5))])
                   3)))

(defn draw-cursor-mode [g]
  (draw-cursor g @cursor-pos)
  (doseq [[k v] @lels]
    (case (v :type)
      dff (draw-dff g v (if (@selected k) Color/RED Color/BLACK))
      mux21 (draw-mux21 g v (if (@selected k) Color/RED Color/BLACK))
      )))

(defn draw-dff-mode [g]
  (doseq [[k v] @lels]
    (case (v :type)
      dff (draw-dff g v Color/BLACK)
      mux21 (draw-mux21 g v Color/BLACK)))
  (draw-dff g @cursor-pos Color/RED))

(defn draw-mux21-mode [g]
  (doseq [[k v] @lels]
    (case (v :type)
      dff (draw-dff g v Color/BLACK)
      mux21 (draw-mux21 g v Color/BLACK)))
  (draw-mux21 g @cursor-pos Color/RED))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-status g [@cursor-pos @mode @lels @selected])
      (case @mode
        cursor (draw-cursor-mode g)
        move (draw-cursor-mode g)
        dff (draw-dff-mode g)
        mux21 (draw-mux21-mode g)))
    (getPreferredSize []
      (Dimension. 800 400))))

(defn move-cursor [dir]
  (dosync
    (ref-set cursor-pos
             (case dir
               left  (assoc @cursor-pos :x (dec (@cursor-pos :x)))
               right (assoc @cursor-pos :x (inc (@cursor-pos :x)))
               up    (assoc @cursor-pos :y (dec (@cursor-pos :y)))
               down  (assoc @cursor-pos :y (inc (@cursor-pos :y)))
               ))))

(defn move-selected [dir]
  (dosync
    (move-cursor dir)
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
                     @selected))))

(defn close-window [frame]
  (let [yn (JOptionPane/showConfirmDialog
            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
    (when (= yn JOptionPane/YES_OPTION)
      (.dispose frame))))

(defn release-selection []
  (dosync
    (ref-set selected #{})))

(defn find-lel-by-pos [lels pos]
  (some (fn [[k v]]
          (when (and (= (pos :x) (v :x))
                     (= (pos :y) (v :y)))
            k))
        lels))

(defn remove-lel-by-key [lels keys]
  (apply hash-map
         (apply concat
                (remove (fn [[k v]] (keys k))
                        lels))))

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
   KeyEvent/VK_A      (fn [_] (dosync
                                (release-selection)
                                (ref-set mode 'dff)))
   KeyEvent/VK_B      (fn [_] (dosync
                                (release-selection)
                                (ref-set mode 'mux21)))
   KeyEvent/VK_M      (fn [_] (dosync
                                (ref-set mode 'move)))
   KeyEvent/VK_ENTER
   (fn [_]
     (let [lel-key (find-lel-by-pos @lels @cursor-pos)]
       (when lel-key
         (dosync
           (alter selected conj lel-key)
           ))))
   KeyEvent/VK_ESCAPE (fn [_] (release-selection))
   KeyEvent/VK_X      (fn [_] (dosync
                                (alter lels remove-lel-by-key @selected)
                                (ref-set selected #{})
                                ))})

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
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode 'cursor)))
   KeyEvent/VK_B      (fn [_] (dosync (ref-set mode 'mux21)))
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
   KeyEvent/VK_ESCAPE (fn [_] (dosync (ref-set mode 'cursor)))
   KeyEvent/VK_A      (fn [_] (dosync (ref-set mode 'dff)))
   })

(def key-command-move-mode
  {KeyEvent/VK_LEFT   (fn [_] (move-selected 'left))
   KeyEvent/VK_RIGHT  (fn [_] (move-selected 'right))
   KeyEvent/VK_UP     (fn [_] (move-selected 'up))
   KeyEvent/VK_DOWN   (fn [_] (move-selected 'down))
   KeyEvent/VK_H      (fn [_] (move-selected 'left))
   KeyEvent/VK_L      (fn [_] (move-selected 'right))
   KeyEvent/VK_K      (fn [_] (move-selected 'up))
   KeyEvent/VK_J      (fn [_] (move-selected 'down))
   KeyEvent/VK_Q      (fn [{frame :frame}] (close-window frame))
   KeyEvent/VK_ESCAPE (fn [_] (dosync
                                (release-selection)
                                (ref-set mode 'cursor)))
   })

(def key-command
  {'cursor key-command-cursor-mode
   'dff    key-command-dff-mode
   'mux21  key-command-mux21-mode
   'move   key-command-move-mode
   })

(defn make-key-lis [frame panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [f ((key-command @mode) (.getKeyCode e))]
        (when f (f {:frame frame})))
      (.repaint panel))
    (keyReleased [e])
    (keyTyped [e])))

(defn -main []
  (let [frame (JFrame. "various")
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
