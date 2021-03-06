(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(require 'clojure.set)

(def dffs (ref #{{:x 2 :y 2} {:x 10 :y 10}}))
(def selected (ref #{}))

(def cursor-pos (ref {:x 5 :y 5}))
(def pix-per-grid 10)
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

(defn draw-dff [g dff color]
  (let [[x y] (map #(* pix-per-grid (dff %)) [:x :y])]
    (.setColor g color)
    (.drawPolygon g
                  (int-array [x x (+ x 40) (+ x 40)])
                  (int-array [y (+ y 50) (+ y 50) y])
                  4)
    (.drawPolyline g
                   (int-array [(+ x 10) (+ x 20) (+ x 30)])
                   (int-array [(+ y 50) (+ y 40) (+ y 50)])
                   3)))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-status g [@cursor-pos @mode @dffs @selected])
      (case @mode
        cursor (do
                 (draw-cursor g @cursor-pos)
                 (doseq [dff @dffs]
                   (draw-dff g dff Color/BLACK))
                 (doseq [dff @selected]
                   (draw-dff g dff Color/RED)))
        dff (do
              (doseq [dff @dffs]
                (draw-dff g dff Color/BLACK))
              (draw-dff g @cursor-pos Color/RED)
              )))
    (getPreferredSize []
      (Dimension. 400 400))))

(defn move-cursor [dir]
  (dosync
    (ref-set cursor-pos
             (case dir
               left  (assoc @cursor-pos :x (dec (@cursor-pos :x)))
               right (assoc @cursor-pos :x (inc (@cursor-pos :x)))
               up    (assoc @cursor-pos :y (dec (@cursor-pos :y)))
               down  (assoc @cursor-pos :y (inc (@cursor-pos :y)))
               ))))

(defn close-window [frame]
  (let [yn (JOptionPane/showConfirmDialog
            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
    (when (= yn JOptionPane/YES_OPTION)
      (.dispose frame))))

(defn release-selection []
  (dosync
    (alter dffs clojure.set/union @selected)
    (ref-set selected #{})))

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
    KeyEvent/VK_ENTER  (fn [& _]
                         (let [dff (@dffs @cursor-pos)]
                           (when dff
                             (dosync
                               (alter dffs disj dff)
                               (alter selected conj dff)
                               ))))
    KeyEvent/VK_ESCAPE (fn [& _] (release-selection))
    KeyEvent/VK_X      (fn [& _] (dosync (ref-set selected #{})))}
   'dff
   {KeyEvent/VK_LEFT   (fn [& _] (move-cursor 'left))
    KeyEvent/VK_RIGHT  (fn [& _] (move-cursor 'right))
    KeyEvent/VK_UP     (fn [& _] (move-cursor 'up))
    KeyEvent/VK_DOWN   (fn [& _] (move-cursor 'down))
    KeyEvent/VK_H      (fn [& _] (move-cursor 'left))
    KeyEvent/VK_L      (fn [& _] (move-cursor 'right))
    KeyEvent/VK_K      (fn [& _] (move-cursor 'up))
    KeyEvent/VK_J      (fn [& _] (move-cursor 'down))
    KeyEvent/VK_ENTER  (fn [& _] (dosync (alter dffs conj @cursor-pos)))
    KeyEvent/VK_ESCAPE (fn [& _] (dosync (ref-set mode 'cursor)))
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
  (let [frame (JFrame. "add_dff")
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
