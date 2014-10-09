(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension Font])
(import '[javax.swing JFrame JPanel JOptionPane])
(import '[java.awt.event KeyListener KeyEvent])

(def dffs (atom '[[2 2] [10 10]]))

;(def direction (atom 'up))

(def cursor-pos (atom [5 5]))
(def pix-per-grid 10)
(def mode (atom 'cursor))

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

(defn draw-dff [g dff color]
  (let [[x y] (map #(* pix-per-grid (dff %)) [0 1])]
    (.setColor g color)
    (.drawPolygon g
                  (int-array [x x (+ x 40) (+ x 40)])
                  (int-array [y (+ y 50) (+ y 50) y])
                  4)))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-status g [@cursor-pos @mode @dffs])
      (doseq [dff @dffs] (draw-dff g dff Color/BLACK))
      (case @mode
        cursor (draw-cursor g @cursor-pos)
        dff    (draw-dff g @cursor-pos Color/RED)))
    (getPreferredSize []
      (Dimension. 400 400))))

(defn move-cursor [dir]
  (reset! cursor-pos
          (case dir
            left  (assoc @cursor-pos 0 (dec (@cursor-pos 0)))
            right (assoc @cursor-pos 0 (inc (@cursor-pos 0)))
            up    (assoc @cursor-pos 1 (dec (@cursor-pos 1)))
            down  (assoc @cursor-pos 1 (inc (@cursor-pos 1))))))

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
              (= code KeyEvent/VK_M)                    (change-mode)
              (= code KeyEvent/VK_Q)                    (close-window frame))
        (when (and (= @mode 'dff)
                   (= code KeyEvent/VK_ENTER))
          (swap! dffs conj @cursor-pos)))
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
