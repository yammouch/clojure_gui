(ns gui
  (:gen-class
   :init init
   :state state))

(import '(java.awt Color Dimension))
(import '(javax.swing JFrame JPanel))

(defn -init []
  [[] (atom [])])

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (.setColor g Color/BLUE)
      (.drawPolyline g (int-array [ 20 220])
                       (int-array [220  20])
                       2))
    (getPreferredSize []
      (Dimension. 240 240))))

(defn anime-panel []
  (let [frame (JFrame. "gui")
        panel (make-panel)]
    (doto frame
      (.add panel)
      (.pack)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    panel))

(defn -main []
  (let [panel (anime-panel)]
    ))
