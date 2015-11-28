(ns mouse 
  (:gen-class
   :init init
   :state state))

(import '(java.awt Color Dimension BorderLayout GridLayout Font BasicStroke))
(import '(java.awt.event ActionListener MouseEvent
                         MouseListener MouseMotionListener))
(import '(javax.swing JFrame JPanel JButton))

(defn -init []
  [[] (atom [])])

(def mouse-pos (ref [0 0]))
(def cursor-pos (ref [0 0 :right]))
(def mode (ref :add-line))
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

(defn calc-cursor-pos [[x y]]
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

(defn update-cursor [#^{:tag MouseEvent} e]
  (let [x (.getX e) y (.getY e)
        [_ _ dir :as cp] (calc-cursor-pos [x y])]
    (dosync
      (ref-set mouse-pos [x y])
      (when (not= dir :stay) (ref-set cursor-pos cp))
      )))

(defn make-mouse-listener [panel]
  (proxy [MouseListener] []
    (mousePressed [e]
      (update-cursor e)
      (println @cursor-pos))
    (mouseClicked [_])
    (mouseReleased [_])
    (mouseEntered [_])
    (mouseExited [_])))

(defn make-mouse-motion-listener [panel]
  (proxy [MouseMotionListener] []
    (mouseMoved [e]
      (update-cursor e)
      (.repaint panel))
    (mouseDragged [e]
      (update-cursor e)
      (.repaint panel)
      (println @cursor-pos)
      )))

(let [stroke (BasicStroke. 3)]
  (defn draw-cursor [g [gx gy dir]]
    (let [x0 (+ (get offset 0) (* gx interval))
          y0 (+ (get offset 1) (* gy interval))
          [x1 y1] (map + [x0 y0]
                         (case dir :up    [0 (- interval-half)]
                                   :down  [0 interval-half    ]
                                   :left  [(- interval-half) 0]
                                   :right [interval-half     0]))]
      (.setStroke g stroke)
      (.setColor g Color/BLUE)
      (.drawLine g x0 y0 x1 y1))))

(let [font (Font. Font/MONOSPACED Font/PLAIN 12)]
  (defn draw-status [g]
    (.setColor g Color/BLUE)
    (.setFont g font)
    (.drawString g (str @mouse-pos) 10 10)
    (.drawString g (str @cursor-pos) 110 10)
    (.drawString g (str @mode) 210 10)))

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
      (draw-status g)
      (draw-cursor g @cursor-pos))
    (getPreferredSize []
      (Dimension. (+ (* (get offset 0) 2) (* interval (get field-size 0)))
                  (+ (* (get offset 1) 2) (* interval (get field-size 1)))
                  ))))

(defn make-button-panel [schem-panel]
  (let [b-add-line (JButton. "add line")
        b-del-line (JButton. "delete line")
        panel (JPanel.)]
    (.addActionListener b-add-line
     (proxy [ActionListener] []
       (actionPerformed [_]
         (dosync (ref-set mode :add-line))
         (.repaint schem-panel))))
    (.addActionListener b-del-line
     (proxy [ActionListener] []
       (actionPerformed [_]
         (dosync (ref-set mode :del-line))
         (.repaint schem-panel))))
    (.setLayout panel (GridLayout. 2 1))
    (.add panel b-add-line)
    (.add panel b-del-line)
    panel))

(defn anime-panel []
  (let [frame (JFrame. "Mouse Motion")
        schem-panel (make-panel)
        button-panel (make-button-panel schem-panel)
        mouse-listener (make-mouse-listener schem-panel)
        mouse-motion-listener (make-mouse-motion-listener schem-panel)]
    (.addMouseListener schem-panel mouse-listener)
    (.addMouseMotionListener schem-panel mouse-motion-listener)
    (.. frame getContentPane (add button-panel BorderLayout/WEST))
    (.. frame getContentPane (add schem-panel BorderLayout/CENTER))
    (.pack frame)
    ;(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setVisible frame true)
    frame))

(defn -main []
  (let [panel (anime-panel)]
    ))
