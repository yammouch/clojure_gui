(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(def dffs (atom #{{:x 2 :y 2} {:x 10 :y 10}}))

(def cursor-pos (atom {:x 5 :y 5}))
(def pix-per-grid 10)
(def mode (atom 'cursor))

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
      (draw-status g [@cursor-pos @mode @dffs])
      (doseq [dff @dffs] (draw-dff g dff Color/BLACK))
      (cond (= @mode 'cursor) (draw-cursor g @cursor-pos)
            (= @mode 'dff)    (draw-dff g @cursor-pos Color/RED)
            (and (seq? @mode)
                 (= (first @mode) 'selected))
            (do
              (draw-cursor g @cursor-pos)
              (draw-dff g (second @mode) Color/RED)
              )))
    (getPreferredSize []
      (Dimension. 400 400))))

(defn move-cursor [dir]
  (reset! cursor-pos
          (case dir
            left  (assoc @cursor-pos :x (dec (@cursor-pos :x)))
            right (assoc @cursor-pos :x (inc (@cursor-pos :x)))
            up    (assoc @cursor-pos :y (dec (@cursor-pos :y)))
            down  (assoc @cursor-pos :y (inc (@cursor-pos :y))))))

(defn change-mode []
  (cond (= @mode 'cursor) (reset! mode 'dff)
        (= @mode 'dff)    (reset! mode 'cursor)))

(defn close-window [frame]
  (let [yn (JOptionPane/showConfirmDialog
            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
    (when (= yn JOptionPane/YES_OPTION)
      (.dispose frame))))

(defn make-key-lis [frame panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [code (.getKeyCode e)]
        (cond (#{KeyEvent/VK_LEFT  KeyEvent/VK_H} code) (move-cursor 'left)
              (#{KeyEvent/VK_RIGHT KeyEvent/VK_L} code) (move-cursor 'right)
              (#{KeyEvent/VK_UP    KeyEvent/VK_K} code) (move-cursor 'up)
              (#{KeyEvent/VK_DOWN  KeyEvent/VK_J} code) (move-cursor 'down)
              (= code KeyEvent/VK_Q)                    (close-window frame))
        (cond (= @mode 'cursor)
              (cond (= code KeyEvent/VK_A)
                    (reset! mode 'dff)
                    (and (= code KeyEvent/VK_ENTER)
                         (@dffs @cursor-pos))
                    (reset! mode (list 'selected @cursor-pos))
                    ))
        (cond (= @mode 'dff)
              (cond (= code KeyEvent/VK_ENTER)
                    (swap! dffs conj @cursor-pos)
                    (= code KeyEvent/VK_ESCAPE)
                    (reset! mode 'cursor)
                    ))
        (cond (and (seq? @mode)
                   (= (first @mode) 'selected))
              (cond (= code KeyEvent/VK_X)
                    (do
                      (swap! dffs disj (second @mode))
                      (reset! mode 'cursor))
                    (= code KeyEvent/VK_ESCAPE)
                    (reset! mode 'cursor))))
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
