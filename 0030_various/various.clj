(ns various
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font])
(import '[java.awt.font TextLayout])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(require 'clojure.set)

(def lels (ref {[2 2] 'dff [10 10] 'mux21}))
(def selected (ref {}))

(def cursor-pos (ref [5 5]))
(def pix-per-grid 8)
(def mode (ref 'cursor))

(defn -init []
  [[] (atom [])])

(defn draw-cursor [g pos]
  (let [cursor-size 10
        x (- (* (pos 0) pix-per-grid)
             (* 0.5 cursor-size))
        y (- (* (pos 1) pix-per-grid)
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
        [x y] (map #(* grid (pos %)) [0 1])
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
        [x y] (map #(* grid (pos %)) [0 1])]
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
  (doseq [[pos type] @lels]
    (case type
      dff (draw-dff g pos Color/BLACK)
      mux21 (draw-mux21 g pos Color/BLACK)))
  (doseq [[pos type] @selected]
    (case type
      dff (draw-dff g pos Color/RED)
      mux21 (draw-mux21 g pos Color/RED))))

(defn draw-dff-mode [g]
  (doseq [[pos type] @lels]
    (case type
      dff (draw-dff g pos Color/BLACK)
      mux21 (draw-mux21 g pos Color/BLACK)))
  (draw-dff g @cursor-pos Color/RED))

(defn draw-mux21-mode [g]
  (doseq [[pos type] @lels]
    (case type
      dff (draw-dff g pos Color/BLACK)
      mux21 (draw-mux21 g pos Color/BLACK)))
  (draw-mux21 g @cursor-pos Color/RED))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-status g [@cursor-pos @mode @lels @selected])
      (case @mode
        cursor (draw-cursor-mode g)
        dff (draw-dff-mode g)
        mux21 (draw-mux21-mode g)))
    (getPreferredSize []
      (Dimension. 400 400))))

(defn move-cursor [dir]
  (dosync
    (ref-set cursor-pos
             (case dir
               left  (assoc @cursor-pos 0 (dec (@cursor-pos 0)))
               right (assoc @cursor-pos 0 (inc (@cursor-pos 0)))
               up    (assoc @cursor-pos 1 (dec (@cursor-pos 1)))
               down  (assoc @cursor-pos 1 (inc (@cursor-pos 1)))
               ))))

(defn close-window [frame]
  (let [yn (JOptionPane/showConfirmDialog
            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
    (when (= yn JOptionPane/YES_OPTION)
      (.dispose frame))))

(defn release-selection []
  (dosync
    (alter lels conj @selected)
    (ref-set selected {})))

(def key-command
  {'cursor
   {KeyEvent/VK_LEFT   (fn [& _] (move-cursor 'left))
    KeyEvent/VK_RIGHT  (fn [& _] (move-cursor 'right))
    KeyEvent/VK_UP     (fn [& _] (move-cursor 'up))
    KeyEvent/VK_DOWN   (fn [& _] (move-cursor 'down))
    KeyEvent/VK_H      (fn [& _] (move-cursor 'left))
    KeyEvent/VK_L      (fn [& _] (move-cursor 'right))
    KeyEvent/VK_K      (fn [& _] (move-cursor 'up))
    KeyEvent/VK_J      (fn [& _] (move-cursor 'down))
    KeyEvent/VK_Q      (fn [frame & _] (close-window frame))
    KeyEvent/VK_A      (fn [& _] (dosync
                                   (release-selection)
                                   (ref-set mode 'dff)))
    KeyEvent/VK_B      (fn [& _] (dosync
                                   (release-selection)
                                   (ref-set mode 'mux21)))
    KeyEvent/VK_ENTER  (fn [& _]
                         (let [lel (@lels @cursor-pos)]
                           (when lel
                             (dosync
                               (alter lels dissoc @cursor-pos)
                               (alter selected conj {@cursor-pos lel})
                               ))))
    KeyEvent/VK_ESCAPE (fn [& _] (release-selection))
    KeyEvent/VK_X      (fn [& _] (dosync (ref-set selected {})))}
   'dff
   {KeyEvent/VK_LEFT   (fn [& _] (move-cursor 'left))
    KeyEvent/VK_RIGHT  (fn [& _] (move-cursor 'right))
    KeyEvent/VK_UP     (fn [& _] (move-cursor 'up))
    KeyEvent/VK_DOWN   (fn [& _] (move-cursor 'down))
    KeyEvent/VK_H      (fn [& _] (move-cursor 'left))
    KeyEvent/VK_L      (fn [& _] (move-cursor 'right))
    KeyEvent/VK_K      (fn [& _] (move-cursor 'up))
    KeyEvent/VK_J      (fn [& _] (move-cursor 'down))
    KeyEvent/VK_Q      (fn [frame & _] (close-window frame))
    KeyEvent/VK_ENTER  (fn [& _] (dosync (alter lels conj {@cursor-pos 'dff})))
    KeyEvent/VK_ESCAPE (fn [& _] (dosync (ref-set mode 'cursor)))
    KeyEvent/VK_B      (fn [& _] (dosync (ref-set mode 'mux21)))}
   'mux21
   {KeyEvent/VK_LEFT   (fn [& _] (move-cursor 'left))
    KeyEvent/VK_RIGHT  (fn [& _] (move-cursor 'right))
    KeyEvent/VK_UP     (fn [& _] (move-cursor 'up))
    KeyEvent/VK_DOWN   (fn [& _] (move-cursor 'down))
    KeyEvent/VK_H      (fn [& _] (move-cursor 'left))
    KeyEvent/VK_L      (fn [& _] (move-cursor 'right))
    KeyEvent/VK_K      (fn [& _] (move-cursor 'up))
    KeyEvent/VK_J      (fn [& _] (move-cursor 'down))
    KeyEvent/VK_Q      (fn [frame & _] (close-window frame))
    KeyEvent/VK_ENTER  (fn [& _] (dosync
                                   (alter lels conj {@cursor-pos 'mux21})))
    KeyEvent/VK_ESCAPE (fn [& _] (dosync (ref-set mode 'cursor)))
    KeyEvent/VK_A      (fn [& _] (dosync (ref-set mode 'dff)))
    }})

(defn make-key-lis [frame panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [f ((key-command @mode) (.getKeyCode e))]
        (when f (f frame)))
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
