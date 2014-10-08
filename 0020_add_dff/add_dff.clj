(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension])
(import '[javax.swing JFrame JPanel])
(import '[java.awt.event KeyListener KeyEvent])

(def dffs '([10 10] [100 100]))

(defn -init []
  [[] (atom [])])

(defn -main []
  (let [frame (JFrame. "add_dff")
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (.setColor g Color/BLACK)
                  (doseq [dff dffs]
                    (let [x (dff 0) y (dff 1)]
                      (.drawPolygon g
                                    (int-array [x x (+ x 40) (+ x 40)])
                                    (int-array [y (+ y 50) (+ y 50) y])
                                    4))))
                (getPreferredSize []
                  (Dimension. 400 400)))
        key-lis (proxy [KeyListener] []
                  (keyPressed [e]
                    (let [code (.getKeyCode e)]
                      (cond (= code KeyEvent/VK_LEFT)  (println 'VK_LEFT)
                            (= code KeyEvent/VK_RIGHT) (println 'VK_RIGHT)
                            (= code KeyEvent/VK_UP)    (println 'VK_UP)
                            (= code KeyEvent/VK_DOWN)  (println 'VK_DOWN)
                            :else                      (println 'other)
                            )))
                  (keyReleased [e])
                  (keyTyped [e]))]
    (do
      (.setFocusable panel true)
      (.addKeyListener panel key-lis)
      (.add frame panel)
      (.pack frame)
      ;(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
      (.setVisible frame true))
    'done))
