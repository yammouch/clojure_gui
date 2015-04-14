(ns LogicElements)

(gen-class
  :name "LogicElements")

(import '(javafx.scene.input KeyCode))

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------

(defn lel-init [type]
  (case type
    :in    {:type :in    :x 0 :y 0 :direction :right}
    :out   {:type :out   :x 0 :y 0 :direction :right}
    :inout {:type :inout :x 0 :y 0 :direction :horizontal}
    :dot   {:type :dot   :x 0 :y 0}
    :not   {:type :not   :x 0 :y 0 :direction :horizontal}
    :buf   {:type :buf   :x 0 :y 0 :direction :right :width 4 :height 4}
    :and   {:type :and   :x 0 :y 0 :direction :right :width 4 :height 4}
    :or    {:type :or    :x 0 :y 0 :direction :right :width 4 :height 4}
    :dff   {:type :dff   :x 0 :y 0 :width 4 :height 5 :async-reset :true}
    :name  {:type :name  :x 0 :y 0
            :string "blah" :v-align :bottom :h-align :left}
    :mux21 {:type :mux21 :x 0 :y 0 :direction :right :width 2 :height 6
            :order01 :0->1}
    :mux-n {:type :mux-n :x 0 :y 0 :direction :right :width 2 :height 6}
    :op    {:type :op    :x 0 :y 0 :width 4 :height 4 :operator "+"}
    ))

(defmulti width  (fn [lel] (:type lel)))
(defmulti height (fn [lel] (:type lel)))
(defmulti x-min  (fn [lel] (:type lel)))
(defmulti x-max  (fn [lel] (:type lel)))
(defmulti y-min  (fn [lel] (:type lel)))
(defmulti y-max  (fn [lel] (:type lel)))

(defmethod width  :default [lel]
  (case (lel :direction)
    (:up :down :vertical) (lel :height)
    (lel :width)))
(defmethod height :default [lel]
  (case (lel :direction)
    (:up :down :vertical) (lel :width)
    (lel :height)))
(defmethod x-min  :default [lel] (:x lel))
(defmethod x-max  :default [lel] (+ (:x lel) (width lel)))
(defmethod y-min  :default [lel] (:y lel))
(defmethod y-max  :default [lel] (+ (:y lel) (height lel)))

; for "in"
(defmethod width  :in [lel]
  (case (lel :direction) (:right :left) 3, (:up :down) 2))
(defmethod height :in [lel]
  (case (lel :direction) (:right :left) 2, (:up :down) 3))

; for "out"
(defmethod width  :out [lel]
  (case (lel :direction) (:right :left) 3, (:up :down) 2))
(defmethod height :out [lel]
  (case (lel :direction) (:right :left) 2, (:up :down) 3))

; for "inout"
(defmethod width  :inout [lel]
  (case (lel :direction) :horizontal 3, :vertical 2))
(defmethod height :inout [lel]
  (case (lel :direction) :horizontal 2, :vertical 3))

; for "dot"
(defmethod width  :dot [lel] 0)
(defmethod height :dot [lel] 0)

; for "name"
; 0 of size is feasible?
(defmethod width  :name [lel] 0)
(defmethod height :name [lel] 0)

; for "not"
(defmethod x-min  :not [lel]
  (case (lel :direction)
    :horizontal (:x lel)
    :vertical   (dec (:x lel))))
(defmethod x-max  :not [lel]
  (case (lel :direction)
    :horizontal (inc (:x lel))
    :vertical   (inc (:x lel))))
(defmethod y-min  :not [lel]
  (case (lel :direction)
    :horizontal (dec (:y lel))
    :vertical   (dec (:y lel))))
(defmethod y-max  :not [lel]
  (case (lel :direction)
    :horizontal (inc (:y lel))
    :vertical   (:y lel)))
(defmethod width  :not [lel]
  (case (lel :direction) :horizontal 1, :vertical 2))
(defmethod height :not [lel]
  (case (lel :direction) :horizontal 2, :vertical 1))

(defn remove-lel-by-key [lels keys]
  (into {} (remove (fn [[k _]] (keys k)) lels)))

(defn remove-geom-by-key [wires keys]
  (into {} (remove (fn [[k _]] (= (keys k) #{:p0 :p1})) wires)))

;--------------------------------------------------
; undo, redo
;--------------------------------------------------
(let [undo-depth 64]
  (defn push-undo [schem]
    (-> schem
        (dissoc :redos)
        (update-in [:undos]
         #(take undo-depth
                (cons {:lels (:lels schem) :geoms (:geoms schem)} %)
                )))))
(defn undo-redo [schem from to]
  (if (empty? (schem from))
    schem
    (-> schem
        (update-in [to] #(cons {:lels (:lels schem) :geoms (:geoms schem)} %))
        (assoc :lels  (:lels  (first (schem from))))
        (assoc :geoms (:geoms (first (schem from))))
        (update-in [from] next))))

;;--------------------------------------------------
;; dialog box
;;--------------------------------------------------
;
;(defn dialog-table [type]
;  (case type
;    (:in :out)    [[:radio :direction :right :up :left :down]]
;    (:inout :not) [[:radio :direction :horizontal :vertical]]
;    (:and :or :buf :mux-n)
;                  [[:edstr :height read-string]
;                   [:edstr :width  read-string]
;                   [:radio :direction :right :up :left :down]]
;    :name         [[:edstr :string identity]
;                   [:radio :h-align :left   :center :right]
;                   [:radio :v-align :bottom :center :top  ]]
;    :mux21        [[:edstr :height read-string]
;                   [:edstr :width  read-string]
;                   [:radio :direction :right :up :left :down]
;                   [:radio :order01 :0->1 :1->0]]
;    :dff          [[:edstr :height read-string]
;                   [:edstr :width  read-string]
;                   [:radio :async-reset :true :false]]
;    :op           [[:edstr :operator identity   ]
;                   [:edstr :height   read-string]
;                   [:edstr :width    read-string]]
;    nil))

(def catalog-table
  [[:in    :out   :inout :dot :not ]
   [:buf   :and   :or    :dff :name]
   [:mux21 :mux-n :op              ]])

(defn cursor-mode [schem]
  (conj {:mode :cursor :selected-lels #{} :selected-geoms {}}
        (select-keys schem [:lels :geoms :cursor-pos :cursor-speed
                            :redos :undos])))

(defn wire-mode [schem]
  (conj {:mode :wire :wire-p0 (:cursor-pos schem)}
        (select-keys schem [:lels :geoms :cursor-pos :cursor-speed
                            :redos :undos])))

(defn add-mode [schem lel-type]
  (conj {:mode :add :lel (lel-init lel-type)}
        (select-keys schem [:lels :geoms :cursor-pos :cursor-speed
                            :redos :undos])))

(defn catalog-mode [schem]
  {:mode :catalog :catalog-pos {:x 0 :y 0} :revert-schem schem})

;--------------------------------------------------
; move lels and geoms
;--------------------------------------------------

(defn move-lels [lels dir speed]
  (reduce #(update-in %1 [%2 dir] (partial + speed)) lels (keys lels)))

(defn move-wire [wire dir speed points]
  (let [[& coords] (case points
                     #{:p0}     (case dir :x [:x0    ] :y [:y0    ])
                     #{:p1}     (case dir :x [    :x1] :y [    :y1])
                     #{:p0 :p1} (case dir :x [:x0 :x1] :y [:y0 :y1]))]
    (reduce #(update-in %1 [%2] (partial + speed))
            wire coords)))

(defn move-geoms [wires dir speed]
  (into {} (map (fn [[k v]] [k (move-wire v dir speed '#{:p0 :p1})])
                wires)))

(defn move-geoms-by-vertices [wires moving-vertices dir speed]
  (reduce-kv (fn [wires k v]
               (update-in wires [k] move-wire dir speed v))
             wires moving-vertices))

(defn move-selected [schem dir speed]
  (-> schem
      (update-in [:moving-lels ] move-lels  dir speed)
      (update-in [:moving-geoms] move-geoms dir speed)
      (update-in [:geoms] move-geoms-by-vertices
                 (:moving-vertices schem) dir speed
                 )))

(defn move-mode [{sl :selected-lels sg :selected-geoms :as schem}]
  (if (and (empty? sl) (empty? sg))
    schem
    (let [{:keys [ml nl]} ; ml: moved lel, nl: not moved lel
           (group-by #(if (sl (% 0)) :ml :nl)
                     (:lels schem))
          {:keys [mg ng]} ; mg: moved geom, ng: not moved geom
           (group-by #(if (= (sg (% 0)) #{:p0 :p1}) :mg :ng)
                     (:geoms schem))]
      (conj {:mode :move, :lels (into {} nl), :geoms (into {} ng)
             :moving-lels     (into {} ml)
             :moving-geoms    (into {} mg)
             :moving-vertices (into {} (filter #(not= #{:p0 :p1} (% 1))
                                               sg))
             :revert-schem    schem}
            (select-keys schem [:cursor-pos :cursor-speed :undos :redos])
            ))))

(defn copy-mode [schem]
  (let [mm (move-mode schem)]
    (if (= :move (:mode mm))
      (conj (dissoc mm :revert-schem)
            {:mode :copy, :moving-vertices {}})
      schem)))

;--------------------------------------------------
; select
;--------------------------------------------------

(defn find-lel-by-pos [lels {posx :x posy :y}]
  (some (fn [[k {x :x y :y}]]
          (when (= [posx posy] [x y]) k))
        lels))

(defn wire-vs-cursor [wire cur]
  (let [fcomp (fn [qc q0 q1]
                (let [[q0 q1 inv] (if (< q0 q1)
                                    [q0 q1 false]
                                    [q1 q0 true ])]
                  (cond (< qc q0) nil
                        (< q1 qc) nil

                        (< (- q1 q0) 4)
                        (cond (= qc q0) (if inv #{:p1} #{:p0})
                              (= qc q1) (if inv #{:p0} #{:p1})
                              :else     #{:p0 :p1})

                        (<= qc (+ q0 1)) (if inv #{:p1} #{:p0})
                        (<= (- q1 1) qc) (if inv #{:p0} #{:p1})
                        :else #{:p0 :p1}
                        )))]
    (cond (= (map #(cur %) [:x :y]) (map #(wire %) [:x0 :y0])) #{:p0}
          (= (map #(cur %) [:x :y]) (map #(wire %) [:x1 :y1])) #{:p1}

          (= (:x cur) (:x0 wire) (:x1 wire))
          (fcomp (:y cur) (:y0 wire) (:y1 wire))

          (= (:y cur) (:y0 wire) (:y1 wire))
          (fcomp (:x cur) (:x0 wire) (:x1 wire))

          :else nil)))

(defn find-geoms-by-pos [wires pos]
  (reduce-kv (fn [acc k v] (if-let [p (wire-vs-cursor v pos)]
                             (conj acc {k p}) acc))
             {} wires))

; An edge of a line should be selected by surrounding it.
(defn rectangular-select [lels wires {x0 :x y0 :y} {x1 :x y1 :y}]
  (let [xmin (min x0 x1) xmax (max x0 x1)
        ymin (min y0 y1) ymax (max y0 y1)
        lels (filter (fn [[k v]]
                       (and (<= xmin (x-min v)) (<= (x-max v) xmax)
                            (<= ymin (y-min v)) (<= (y-max v) ymax)))
                     lels)
        wires (filter (fn [[k {x0 :x0 y0 :y0 x1 :x1 y1 :y1}]]
                        (and (<= xmin (min x0 x1)) (<= (max x0 x1) xmax)
                             (<= ymin (min y0 y1)) (<= (max y0 y1) ymax)))
                      wires)]
    {:lels (set (keys lels))
     :geoms (zipmap (keys wires) (repeat #{:p0 :p1}))
     }))

(defn select
  [{:keys [cursor-pos lels geoms rect-p0 selected-lels selected-geoms]
    :as schem}]
  (let [lel-key (find-lel-by-pos lels cursor-pos)
        geom-keys (find-geoms-by-pos geoms cursor-pos)
        rect-keys (if rect-p0
                    (rectangular-select lels geoms rect-p0 cursor-pos)
                    {})
        sl (reduce into [selected-lels lels (if lel-key #{lel-key} #{})])
        sw (reduce (partial merge-with into)
            [selected-geoms (:geoms rect-keys) geom-keys])]
    (-> (conj schem {:selected-lels sl :selected-geoms sw})
        (dissoc :rect-p0))))

;--------------------------------------------------
; key commands for each mode on schematic panel
;--------------------------------------------------

(defn key-command-cursor-mode [schem keyEvent]
  (cond
   ; cursor -> catalog
   (= (.getCode keyEvent) KeyCode/E) (catalog-mode schem)
   ; cursor -> move
   (= (.getCode keyEvent) KeyCode/M) (move-mode schem)
   ; cursor -> copy
   (= (.getCode keyEvent) KeyCode/C) (copy-mode schem)
   ; cursor -> wire
   (= (.getCode keyEvent) KeyCode/W) (wire-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/R)
   (if (schem :rect-p0)
     (dissoc schem :rect-p0)
     (assoc schem :rect-p0 (schem :cursor-pos)))
   (= (.getCode keyEvent) KeyCode/ENTER) (select schem)
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   (= (.getCode keyEvent) KeyCode/X)
   (-> (cursor-mode schem)
       push-undo
       (update-in [:lels ] remove-lel-by-key  (schem :selected-lels ))
       (update-in [:geoms] remove-geom-by-key (schem :selected-geoms)))
   (and (= (.getCode keyEvent) KeyCode/Z)
        (.isControlDown keyEvent))   (undo-redo schem :undos :redos)
   (and (= (.getCode keyEvent) KeyCode/Y)
        (.isControlDown keyEvent))   (undo-redo schem :redos :undos)
   :else nil)) ; cond, defn

(defn key-command-add-mode [schem keyEvent]
  (cond
   ; add -> catalog
   (= (.getCode keyEvent) KeyCode/E) (catalog-mode schem)
   ; add -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER)
   (-> schem
       push-undo
       (update-in [:lels]
        #(conj % {(gensym) (conj (:lel schem)
                                 {:x (get-in schem [:cursor-pos :x])
                                  :y (get-in schem [:cursor-pos :y])
                                  })})))
   :else nil))

(defn key-command-move-mode [schem keyEvent]
  (cond
   ; move -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (:revert-schem schem)
   (= (.getCode keyEvent) KeyCode/ENTER)
   (-> (cursor-mode schem) push-undo
       (update-in [:lels ] conj (:moving-lels  schem))
       (update-in [:geoms] conj (:moving-geoms schem)))
   :else nil))

(defn key-command-copy-mode [schem keyEvent]
  (cond
   ; move -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER)
   (-> schem push-undo
       (update-in [:lels ] into (map (fn [[k v]] [(gensym) v])
                                     (:moving-lels  schem)))
       (update-in [:geoms] into (map (fn [[k v]] [(gensym) v])
                                     (:moving-geoms schem))))
   :else nil))

(defn key-command-wire-mode
  [{{x0 :x y0 :y} :wire-p0 {x1 :x y1 :y} :cursor-pos :as schem} keyEvent]
  (cond
   ; wire -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER)
   (-> schem push-undo
       (update-in [:geoms] conj {(gensym) {:x0 x0 :y0 y0 :x1 x1 :y1 y1}})
       (assoc :wire-p0 (:cursor-pos schem)))
   :else nil))

(defn key-command-catalog-mode [{{x :x y :y} :catalog-pos :as schem} keyEvent]
  (cond
   ; catalog -> add
   (= (.getCode keyEvent) KeyCode/ENTER)
   (if-let [type (get-in catalog-table [y x])]
     (add-mode (:revert-schem schem) type)
     schem)
   ; catalog -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode (:revert-schem schem))
   :else nil))

(def key-command
  {:cursor  key-command-cursor-mode
   :add     key-command-add-mode
   :move    key-command-move-mode
   :copy    key-command-copy-mode
   :wire    key-command-wire-mode
   :catalog key-command-catalog-mode
   })

;--------------------------------------------------
; move-cursor
;--------------------------------------------------

(defn lels-on-line [dir {cx :x cy :y :as cursor-pos} lels]
  (let [on-h-line #(<= (y-min %) cy (y-max %))
        on-v-line #(<= (x-min %) cx (x-max %))]
    (filter (case dir
              :left  #(and (on-h-line %) (< (x-min %) cx))
              :right #(and (on-h-line %) (< cx (x-max %)))
              :up    #(and (on-v-line %) (< (y-min %) cy))
              :down  #(and (on-v-line %) (< cy (y-max %))))
            lels)))

(defn wires-on-line [dir {cx :x cy :y :as cursor-pos} wires]
  (let [on-h-line #(or (<= (:y0 %) cy (:y1 %)) (<= (:y1 %) cy (:y0 %)))
        on-v-line #(or (<= (:x0 %) cx (:x1 %)) (<= (:x1 %) cx (:x0 %)))]
    (filter (case dir
              :left  #(and (on-h-line %) (< (min (:x0 %) (:x1 %)) cx))
              :right #(and (on-h-line %) (< cx (max (:x0 %) (:x1 %))))
              :up    #(and (on-v-line %) (< (min (:y0 %) (:y1 %)) cy))
              :down  #(and (on-v-line %) (< cy (max (:y0 %) (:y1 %)))))
            wires)))

(defn coordinates [dir lels wires]
  (let [[lelc0 lelc1 wirec0 wirec1]
        (case dir :x [x-min x-max :x0 :x1] :y [y-min y-max :y0 :y1])]
    (concat (map lelc0  lels)  (map lelc1  lels)
            (map wirec0 wires) (map wirec1 wires))))

(defn jump-amount [dir {cx :x cy :y :as cursor-pos} lels wires]
  (let [lol (lels-on-line dir cursor-pos (vals lels))
        wol (wires-on-line dir cursor-pos (vals wires))
        [fil pick move-dir]
        (case dir
          :left  [#(< % cx) #(apply max %) :x]
          :right [#(< cx %) #(apply min %) :x]
          :up    [#(< % cy) #(apply max %) :y]
          :down  [#(< cy %) #(apply min %) :y])
        filtered (filter fil (apply coordinates move-dir
                               (if (and (empty? lol) (empty? wol))
                                 [(vals lels) (vals wires)]
                                 [lol wol])))]
    (if (empty? filtered)
      0
      (- (pick filtered) (move-dir cursor-pos))
      )))

(defn move-cursor [schem dir speed]
  (update-in schem [:cursor-pos dir] #(+ speed %)))

(defn pane-schem-cursor-speed
  [{cursor-speed :cursor-speed :as schem} keyEvent]
  (let [num ({KeyCode/DIGIT0 0, KeyCode/DIGIT1 1, KeyCode/DIGIT2 2,
              KeyCode/DIGIT3 3, KeyCode/DIGIT4 4, KeyCode/DIGIT5 5,
              KeyCode/DIGIT6 6, KeyCode/DIGIT7 7, KeyCode/DIGIT8 8,
              KeyCode/DIGIT9 9, KeyCode/MINUS :-}
             (.getCode keyEvent))]
    (if (and num (#{:cursor :add :wire :move :copy} (:mode schem)))
      (assoc schem :cursor-speed
             (if (= num :-) 0 (+ (* @cursor-speed 10) num)))
      nil)))

(defn pane-schem-cursor-move
  [{:keys [mode cursor-speed cursor-pos lels geoms] :as schem} keyEvent]
  (let [kc (.getCode keyEvent)
        dir (cond (#{KeyCode/LEFT  KeyCode/H} kc) :left
                  (#{KeyCode/RIGHT KeyCode/L} kc) :right
                  (#{KeyCode/UP    KeyCode/K} kc) :up
                  (#{KeyCode/DOWN  KeyCode/J} kc) :down
                  :else                           nil)]
    (when dir
      (let [speed (cond (<= cursor-speed 0)
                        (jump-amount dir cursor-pos lels geoms)
                        ('#{:left :up} dir)  (- cursor-speed)
                        :else                cursor-speed)]
        (case mode
          (:cursor :add :wire) (move-cursor schem dir speed)
          (:move :copy)        (-> (move-cursor schem dir speed)
                                   (move-selected     dir speed))
          :catalog (update-in schem [:catalog-pos dir]
                    (if (neg? speed) dec inc))
          nil)))))
;
;(defn pane-schem-key [f-set-to-parent pane]
;  (proxy [EventHandler] []
;    (handle [keyEvent]
;      (or (pane-schem-goto-dialog  keyEvent pane f-set-to-parent)
;          (pane-schem-cursor-move  keyEvent pane)
;          (pane-schem-cursor-speed keyEvent)
;          (let [f (key-command (:mode @mode))]
;            (when (and f (not= (f keyEvent) :no-consume))
;              (.setAll (.getChildren pane) (draw-mode))
;              (.consume keyEvent))))
;      (.setText *label-debug* (state-text))
;      )))


