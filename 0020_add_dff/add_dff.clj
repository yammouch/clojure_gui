(ns add-dff
  (:gen-class
   :init init
   :state state))

(import '[java.awt Color Dimension])
(import '[javax.swing JFrame JPanel])

(def dffs '([10 10] [100 100]))

(defn -init []
  [[] (atom [])])

(defn -main []
  (let [frame (JFrame. "add_dff")
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (.setColor g Color/BLACK)
                  (doseq [dff dffs]
                    (let [x (dff 0) y (dff 1)]
                      (clojure.pprint/pprint dff)
                      (.drawPolygon g
                                    (int-array [x x (+ x 40) (+ x 40)])
                                    (int-array [y (+ y 50) (+ y 50) y])
                                    4))))
                (getPreferredSize []
                  (Dimension. 400 400)))]
    (do
      (.add frame panel)
      (.pack frame)
      ;(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
      (.setVisible frame true))
    'done))
