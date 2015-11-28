(ns mouse 
  (:gen-class
   :init init
   :state state))

(import '(java.awt Color Dimension))
(import '(java.awt.event MouseMotionListener))
(import '(javax.swing JFrame JPanel))

(defn -init []
  [[] (atom [])])

(def mouse-pos (ref [0 0]))
(def cursor-pos (ref [0 0 :right]))
(def offset [30 30])
(def free-area 5)
(def interval-half 10)
(def interval (* interval-half 2))
(def field-size [20 20])

(defn calc-grid [[x y]]
  (let [gx (int (Math/floor (/ (- (+ x interval-half)
                                  (get offset 0))
                               interval)))
        gy (int (Math/floor (/ (- (+ y interval-half)
                                  (get offset 0))
                               interval)))]
    [gx gy
     (- x (* gx interval) (get offset 0))
     (- y (* gy interval) (get offset 1))]))

(defn calc-local-direction [[lx ly]]
  (let [down-left (<= lx ly)
        down-right (<= 0 (+ lx ly))]
    (if down-left
      (if down-right :down  :left)
      (if down-right :right :up))))

(defn calc-cursor-pos [x y]
  (let [[gx gy lx ly] (calc-grid [x y])]
    (if (or (and (<= (- free-area) lx) (< lx free-area)
                 (<= (- free-area) ly) (< ly free-area))
            (< gx 0) (< (get field-size 0) gx)
            (< gy 0) (< (get field-size 1) gy))
      [nil nil :stay]
      (case (calc-local-direction [lx ly])
        :down  (if (not= gy (get field-size 1))
                 [gx  gy  :down ] [nil nil :stay])
        :up    (if (not= gy 0)
                 [gx  gy  :up   ] [nil nil :stay])
        :right (if (not= gx (get field-size 0))
                 [gx  gy  :right] [nil nil :stay])
        :left  (if (not= gx 0)
                 [gx  gy  :left ] [nil nil :stay])))))

(defn make-mouse-listener [panel]
  (proxy [MouseMotionListener] []
    (mouseMoved [e]
      (let [x (.getX e)
            y (.getY e)
            [_ _ dir :as cp] (calc-cursor-pos x y)]
        (dosync
          (ref-set mouse-pos [x y])
          (when (not= dir :stay) (ref-set cursor-pos cp))
          ))
      (.repaint panel))
    ;(mouseDragged [e])
    ))

(defn draw-cursor [g [gx gy dir]]
  (let [x0 (+ (get offset 0) (* gx interval))
        y0 (+ (get offset 1) (* gy interval))
        [x1 y1] (map + [x0 y0]
                       (case dir :up    [0 (- interval-half)]
                                 :down  [0 interval-half    ]
                                 :left  [(- interval-half) 0]
                                 :right [interval-half     0]))]
    (.setColor g Color/CYAN)
    (.drawLine g x0 y0 x1 y1)))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (.setColor g Color/GRAY)
      (let [xs (take (inc (get field-size 0))
                     (iterate #(+ % interval) (get offset 0)))
            x0 (first xs) x1 (last xs)
            ys (take (inc (get field-size 1))
                     (iterate #(+ % interval) (get offset 1)))
            y0 (first ys) y1 (last ys)]
        (doseq [x xs] (.drawLine g x  y0 x  y1))
        (doseq [y ys] (.drawLine g x0 y  x1 y )))
      (draw-cursor g @cursor-pos)
      (.setColor g Color/BLUE)
      (.drawString g (str @mouse-pos) 10 10)
      (.drawString g (str @cursor-pos) 10 22))
    (getPreferredSize []
      (Dimension. (+ (* (get offset 0) 2) (* interval (get field-size 0)))
                  (+ (* (get offset 1) 2) (* interval (get field-size 1)))
                  ))))

(defn anime-panel []
  (let [frame (JFrame. "Mouse Motion")
        panel (make-panel)
        listener (make-mouse-listener panel)]
    (.addMouseMotionListener panel listener)
    (doto frame
      (.add panel)
      (.pack)
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    panel))

(defn -main []
  (let [panel (anime-panel)]
    ))
