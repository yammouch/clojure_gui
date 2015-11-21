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
      (.setColor g Color/BLUE)
      (.drawString g (str @mouse-pos) 10 10)
      (.drawPolyline g (int-array [ 20 220])
                       (int-array [220  20])
                       2))
    (getPreferredSize []
      (Dimension. 240 240))))

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
