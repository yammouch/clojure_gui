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
(def offset [30 30])
(def interval 20)
(def size [20 20])

(defn make-mouse-listener [panel]
  (proxy [MouseMotionListener] []
    (mouseMoved [e]
      (let [x (.getX e)
            y (.getY e)]
        (dosync (ref-set mouse-pos [x y])))
      (.repaint panel))
    ;(mouseDragged [e])
    ))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (.setColor g Color/GRAY)
      (let [xs (take (get size 0)
                     (iterate #(+ % interval) (get offset 0)))
            x0 (first xs) x1 (last xs)
            ys (take (get size 1)
                     (iterate #(+ % interval) (get offset 1)))
            y0 (first ys) y1 (last ys)]
        (doseq [x xs] (.drawLine g x  y0 x  y1))
        (doseq [y ys] (.drawLine g x0 y  x1 y )))
      (.setColor g Color/BLUE)
      (.drawString g (str @mouse-pos) 10 10))
    (getPreferredSize []
      (Dimension. (+ (* (get offset 0) 2) (* interval (get size 0)))
                  (+ (* (get offset 1) 2) (* interval (get size 1)))
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
