(ns LogicElements)

(gen-class
  :name "LogicElements")

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------

(defn lel-init [type]
  (case type
    :in    {:type :in    :x 0 :y 0 :direction :right}
    :out   {:type :out   :x 0 :y 0 :direction :right}
    :inout {:type :inout :x 0 :y 0 :direction :horizontal}
    :dot   {:type :dot   :x 0 :y 0}
    :name  {:type :name  :x 0 :y 0
            :string "blah" :v-align :bottom :h-align :left}
    :not   {:type :not   :x 0 :y 0 :direction :right}
    :and   {:type :and   :x 0 :y 0 :direction :right :width 4 :height 4}
    :or    {:type :or    :x 0 :y 0 :direction :right :width 4 :height 4}
    :dff   {:type :dff   :x 0 :y 0}
    :dffr  {:type :dffr  :x 0 :y 0}
    :mux21 {:type :mux21 :x 0 :y 0 :direction :right :width 2 :height 6
            :order01 :0->1}
    :mux-n {:type :mux-n :x 0 :y 0 :direction :right :width 2 :height 6}
    :plus  {:type :plus  :x 0 :y 0}
    :minus {:type :minus :x 0 :y 0}
    ))

(defmulti lel-width  (fn [lel] (:type lel)))
(defmulti lel-height (fn [lel] (:type lel)))
(defmulti lel-x-min  (fn [lel] (:type lel)))
(defmulti lel-x-max  (fn [lel] (:type lel)))
(defmulti lel-y-min  (fn [lel] (:type lel)))
(defmulti lel-y-max  (fn [lel] (:type lel)))

(defmethod lel-width  :default [lel]
  (case (lel :direction)
    (:up :down :vertical) (lel :height)
    (lel :width)))
(defmethod lel-height :default [lel]
  (case (lel :direction)
    (:up :down :vertical) (lel :width)
    (lel :height)))
(defmethod lel-x-min  :default [lel] (:x lel))
(defmethod lel-x-max  :default [lel] (+ (:x lel) (lel-width lel)))
(defmethod lel-y-min  :default [lel] (:y lel))
(defmethod lel-y-max  :default [lel] (+ (:y lel) (lel-height lel)))

; for "in"
(defmethod lel-width  :in [lel]
  (case (lel :direction) (:right :left) 3, (:up :down) 2))
(defmethod lel-height :in [lel]
  (case (lel :direction) (:right :left) 2, (:up :down) 3))

; for "out"
(defmethod lel-width  :out [lel]
  (case (lel :direction) (:right :left) 3, (:up :down) 2))
(defmethod lel-height :out [lel]
  (case (lel :direction) (:right :left) 2, (:up :down) 3))

; for "inout"
(defmethod lel-width  :inout [lel]
  (case (lel :direction) :horizontal 3, :vertical 2))
(defmethod lel-height :inout [lel]
  (case (lel :direction) :horizontal 2, :vertical 3))

; for "dot"
(defmethod lel-width  :dot [lel] 0)
(defmethod lel-height :dot [lel] 0)

; for "name"
; 0 of size is feasible?
(defmethod lel-width  :name [lel] 0)
(defmethod lel-height :name [lel] 0)

; for "not"
(defmethod lel-width  :not [lel]
  (case (lel :direction) (:right :left) 3, (:up :down) 4))
(defmethod lel-height :not [lel]
  (case (lel :direction) (:right :left) 4, (:up :down) 3))

; for "dff"
(defmethod lel-width  :dff [lel] 4)
(defmethod lel-height :dff [lel] 5)

; for "dffr"
(defmethod lel-width  :dffr [lel] 4)
(defmethod lel-height :dffr [lel] 5)

; for "plus"
(defmethod lel-width  :plus [lel] 4)
(defmethod lel-height :plus [lel] 4)

; for "minus"
(defmethod lel-width  :minus [lel] 4)
(defmethod lel-height :minus [lel] 4)

;--------------------------------------------------
; move-*
;--------------------------------------------------

(defn move-lels [lels dir speed]
  (reduce #(update-in %1 [%2 dir] (partial + speed)) lels (keys lels)))

(defn move-wire [wire dir speed points]
  (let [[& coords] (case points
                     #{p0}    (case dir :x [:x0    ] :y [:y0    ])
                     #{p1}    (case dir :x [    :x1] :y [    :y1])
                     #{p0 p1} (case dir :x [:x0 :x1] :y [:y0 :y1]))]
    (reduce #(update-in %1 [%2] (partial + speed))
            wire coords)))

(defn move-selected-wires [dir speed]
  (let [moved (reduce (fn [wires [sel points]]
                        (update-in wires [sel] move-wire dir speed points))
                      @wires @moving-vertices)]
    (dosync
      (alter moving-wires
             #(reduce (fn [wires k]
                        (update-in wires [k] move-wire dir speed '#{p0 p1}))
                      % (keys %)))
      (ref-set wires moved)
      )))

(defn move-selected [dir speed]
  (dosync
    (alter moving-lels move-lels dir speed))
  (move-selected-wires dir speed))

(defn find-lel-by-pos [lels pos]
  (some (fn [[k v]]
          (when (= (map #(pos %) [:x :y]) (map #(v %) [:x :y])) k))
        lels))

(defn wire-vs-cursor [wire cur]
  (let [fcomp (fn [qc q0 q1]
                (let [[q0 q1 inv] (if (< q0 q1)
                                    [q0 q1 false]
                                    [q1 q0 true ])]
                  (cond (< qc q0) nil
                        (< q1 qc) nil

                        (< (- q1 q0) 4)
                        (cond (= qc q0) (if inv #{'p1} #{'p0})
                              (= qc q1) (if inv #{'p0} #{'p1})
                              :else     #{'p0 'p1})

                        (<= qc (+ q0 1)) (if inv #{'p1} #{'p0})
                        (<= (- q1 1) qc) (if inv #{'p0} #{'p1})
                        :else #{'p0 'p1}
                        )))]
    (cond (= (map #(cur %) [:x :y]) (map #(wire %) [:x0 :y0])) #{'p0}
          (= (map #(cur %) [:x :y]) (map #(wire %) [:x1 :y1])) #{'p1}

          (= (:x cur) (:x0 wire) (:x1 wire))
          (fcomp (:y cur) (:y0 wire) (:y1 wire))

          (= (:y cur) (:y0 wire) (:y1 wire))
          (fcomp (:x cur) (:x0 wire) (:x1 wire))

          :else nil)))

(defn find-wires-by-pos [wires pos]
  (reduce-kv (fn [acc k v] (if-let [p (wire-vs-cursor v pos)]
                             (conj acc {k p}) acc))
             {} wires))

(defn merge-selected-wire [base add]
  (reduce-kv (fn [acc k points]
               (if (base k)
                 (update-in acc [k] #(into % points))
                 (conj acc {k points})))
             base add))

; An edge of a line should be selected by surrounding it.
(defn rectangular-select [lels wires x0 y0 x1 y1]
  (let [xmin (min x0 x1) xmax (max x0 x1)
        ymin (min y0 y1) ymax (max y0 y1)
        lels (filter (fn [[k v]]
                       (and (<= xmin (lel-x-min v)) (<= (lel-x-max v) xmax)
                            (<= ymin (lel-y-min v)) (<= (lel-y-max v) ymax)))
                     lels)
        wires (filter (fn [[k v]]
                        (and (<= xmin (min (:x0 v) (:x1 v)))
                             (<= (max (:x0 v) (:x1 v)) xmax)
                             (<= ymin (min (:y0 v) (:y1 v)))
                             (<= (max (:y0 v) (:y1 v)) ymax)))
                      wires)]
    {:lels (set (keys lels))
     :wires (zipmap (keys wires) (repeat '#{p0 p1}))
     }))

(defn remove-lel-by-key [lels keys]
  (into {} (remove (fn [[k _]] (keys k)) lels)))

(defn remove-wire-by-key [wires keys]
  (into {} (remove (fn [[k _]] (= (keys k) 'p0p1)) wires)))

(defn jump-amount [dir]
  (let [[fil pick move-dir]
          (case dir
            :left  [#(< % (:x @cursor-pos)) #(apply max %) :x]
            :right [#(< (:x @cursor-pos) %) #(apply min %) :x]
            :up    [#(< % (:y @cursor-pos)) #(apply max %) :y]
            :down  [#(< (:y @cursor-pos) %) #(apply min %) :y])
        [lelc0 lelc1 wirec0 wirec1]
          (case move-dir
            :x [lel-x-min lel-x-max :x0 :x1]
            :y [lel-y-min lel-y-max :y0 :y1]
            nil)
        filtered (filter fil
                         (concat (map lelc0  (vals @lels))
                                 (map lelc1  (vals @lels))
                                 (map wirec0 (vals @wires))
                                 (map wirec1 (vals @wires))))]
    (if (empty? filtered)
      0
      (- (pick filtered) (move-dir @cursor-pos))
      )))

(defn pane-schem-cursor-move [keyEvent pane]
  (let [kc (.getCode keyEvent)
        dir (cond (#{KeyCode/LEFT  KeyCode/H} kc) :left
                  (#{KeyCode/RIGHT KeyCode/L} kc) :right
                  (#{KeyCode/UP    KeyCode/K} kc) :up
                  (#{KeyCode/DOWN  KeyCode/J} kc) :down
                  :else                           nil)
        speed (cond (not dir)            1
                    (<= @cursor-speed 0) (jump-amount dir)
                    ('#{:left :up} dir)  (- @cursor-speed)
                    :else                @cursor-speed)
        op (case (:mode @mode)
             (:cursor :add :wire) #(move-cursor % speed)
             :move                #(do (move-cursor   % speed)
                                      (move-selected % speed))
             :catalog             #(move-catalog % (case dir (:left :up) -1 1))
             nil)]
    (when (and dir op)
      (op (case dir (:left :right) :x :y))
      (.consume keyEvent)
      (.setText *label-debug* (state-text))
      (.setAll (.getChildren pane) (draw-mode))
      true)))
