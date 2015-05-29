(ns swing)

(gen-class
  :name "swing"
  :main true)

(import
  '(javax.swing JFrame JPanel)
  '(java.awt Dimension BorderLayout)
  '(java.awt.event KeyListener KeyEvent))

(def origin (ref [0 0]))
(def grid 8)

(defn draw-square [gc [x y]]
  (.drawRect gc x y grid grid))

(defn draw-circle [gc [x y]]
  (.drawOval gc x y grid grid))

(defn draw-triangle [gc [x y]]
  (.drawPolygon gc
   (int-array [x (+ x grid)         x])
   (int-array [y (+ y (* 0.5 grid)) (+ y grid)])
   3))

(defn draw-and [gc [x y]]
  (.drawPolyline gc
   (int-array [(+ (* 0.5 grid) x) x          x (+ (* 0.5 grid) x)])
   (int-array [(+ y grid)         (+ y grid) y y                 ])
   4)
  (.drawArc gc x y grid grid 90 -180))

(defn draw-objects [gc [xorg yorg]]
  (mapcat (fn [y-in-grid]
            (let [y (+ (* y-in-grid grid) yorg)]
              (map (fn [x fdraw] (fdraw gc [x y]))
                   (take 100 (iterate #(+ % grid) xorg))
                   (drop (mod y-in-grid 4)
                         (cycle [draw-square draw-circle
                                 draw-triangle draw-and])))))
          (range 100)))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(def key-commands
  {KeyEvent/VK_RIGHT (fn [] (alter origin update-in [0] #(+ % grid)))
   KeyEvent/VK_LEFT  (fn [] (alter origin update-in [0] #(- % grid)))
   KeyEvent/VK_UP    (fn [] (alter origin update-in [1] #(- % grid)))
   KeyEvent/VK_DOWN  (fn [] (alter origin update-in [1] #(+ % grid)))})

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [gc]
      (proxy-super paintComponent gc)
      (dorun (draw-objects gc @origin)))
    (getPreferredSize []
      (Dimension. 1024 768))))
      
(defn pane-schem-key [panel]
  (proxy [KeyListener] []
    (keyPressed [e]
      (println e)
      (let [f (key-commands (.getKeyCode e))]
        (when f
          (dosync (f))
          (println @origin)
          (.repaint panel))))
    (keyReleased [e])
    (keyTyped [e])))


;--------------------------------------------------
; Swing main routine
;--------------------------------------------------
(defn -main [& args]
  (let [frame (JFrame. "Draws a lot of objects")
        content-pane (.getContentPane frame)
        panel (make-panel)
        key-lis (pane-schem-key panel)]
    (.setFocusable panel true)
    (.addKeyListener panel key-lis)
    (.add content-pane panel BorderLayout/CENTER)
    (.pack frame)
    (.setVisible frame true)
    'done))
