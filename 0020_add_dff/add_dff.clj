(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension])
(import '[javax.swing JFrame JPanel])
(import '[java.awt.event KeyListener KeyEvent])

(def dffs '([10 10] [100 100]))

(def direction (atom 'up))

(def cursor-pos (atom [5 5]))
(def pix-per-grid 10)

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

;(defn draw-arrow [g dir]
;  (let [end-size 10
;        len 50
;        [x0 y0] [200 200]
;        [x1 y1] (case dir
;                  up    [x0 (- y0 len)]
;                  down  [x0 (+ y0 len)]
;                  right [(+ x0 len) y0]
;                  left  [(- x0 len) y0])
;        [x2 y2 x3 y3] (case dir
;                        up    [(- x1 end-size) (+ y1 end-size)
;                               (+ x1 end-size) (+ y1 end-size)]
;                        down  [(- x1 end-size) (- y1 end-size)
;                               (+ x1 end-size) (- y1 end-size)]
;                        right [(- x1 end-size) (+ y1 end-size)
;                               (- x1 end-size) (- y1 end-size)]
;                        left  [(+ x1 end-size) (+ y1 end-size)
;                               (+ x1 end-size) (- y1 end-size)])]
;    (.setColor g Color/BLACK)
;    (.drawPolyline g
;                   (int-array [x0 x1])
;                   (int-array [y0 y1])
;                   2)
;    (.drawPolyline g
;                   (int-array [x2 x1 x3])
;                   (int-array [y2 y1 y3])
;                   3)))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (.setColor g Color/BLACK)
      (doseq [dff dffs]
        (let [x (dff 0) y (dff 1)]
          (.drawPolygon g
                        (int-array [x x (+ x 40) (+ x 40)])
                        (int-array [y (+ y 50) (+ y 50) y])
                        4)))
      (draw-cursor g @cursor-pos))
    (getPreferredSize []
      (Dimension. 400 400))))

(defn make-key-lis [panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (let [code (.getKeyCode e)
            new-pos (cond (= code KeyEvent/VK_LEFT)
                          (assoc @cursor-pos 0 (dec (@cursor-pos 0)))
                          (= code KeyEvent/VK_RIGHT)
                          (assoc @cursor-pos 0 (inc (@cursor-pos 0)))
                          (= code KeyEvent/VK_DOWN)
                          (assoc @cursor-pos 1 (inc (@cursor-pos 1)))
                          (= code KeyEvent/VK_UP)
                          (assoc @cursor-pos 1 (dec (@cursor-pos 1))))]
        (reset! cursor-pos new-pos))
        ;(cond (= code KeyEvent/VK_LEFT)  (reset! direction 'left)
        ;      (= code KeyEvent/VK_RIGHT) (reset! direction 'right)
        ;      (= code KeyEvent/VK_UP)    (reset! direction 'up)
        ;      (= code KeyEvent/VK_DOWN)  (reset! direction 'down))
      (.repaint panel))
    (keyReleased [e])
    (keyTyped [e])))

(defn -main []
  (let [frame (JFrame. "add_dff")
        panel (make-panel)
        key-lis (make-key-lis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener key-lis))
    (doto frame
      (.add panel)
      (.pack)
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    'done))
