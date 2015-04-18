(ns LogicElements)

(gen-class
  :name "LogicElements")

(import '(javafx.scene.input KeyCode))

;--------------------------------------------------
; generic functions for lel (Logic ELement)
;--------------------------------------------------

(defn lel-init [type]
  (case type
    :in    {:type :in    :p [0 0] :direction :right}
    :out   {:type :out   :p [0 0] :direction :right}
    :inout {:type :inout :p [0 0] :direction :horizontal}
    :dot   {:type :dot   :p [0 0]}
    :not   {:type :not   :p [0 0] :direction :horizontal}
    :buf   {:type :buf   :p [0 0] :direction :right :width 4 :height 4}
    :and   {:type :and   :p [0 0] :direction :right :width 4 :height 4}
    :or    {:type :or    :p [0 0] :direction :right :width 4 :height 4}
    :dff   {:type :dff   :p [0 0] :width 4 :height 5 :async-reset :true}
    :name  {:type :name  :p [0 0]
            :string "blah" :v-align :bottom :h-align :left}
    :mux21 {:type :mux21 :p [0 0] :direction :right :width 2 :height 6
            :order01 :0->1}
    :mux-n {:type :mux-n :p [0 0] :direction :right :width 2 :height 6}
    :op    {:type :op    :p [0 0] :width 4 :height 4 :operator "+"}
    ))

(defmulti width  (fn [lel] (:type lel)))
(defmulti height (fn [lel] (:type lel)))
(defmulti x-min  (fn [lel] (:type lel)))
(defmulti x-max  (fn [lel] (:type lel)))
(defmulti y-min  (fn [lel] (:type lel)))
(defmulti y-max  (fn [lel] (:type lel)))

(defmethod width  :default [{d :direction h :height w :width}]
  (case d (:up :down :vertical) h w))
(defmethod height :default [{d :direction h :height w :width}]
  (case d (:up :down :vertical) w h))
(defmethod x-min  :default [{[x _] :p        }]    x             )
(defmethod x-max  :default [{[x _] :p :as lel}] (+ x (width lel)))
(defmethod y-min  :default [{[_ y] :p        }]    y             )
(defmethod y-max  :default [{[_ y] :p :as lel}] (+ y (height lel)))

; for "in"
(defmethod width  :in [{d :direction}] (case d (:right :left) 3 2))
(defmethod height :in [{d :direction}] (case d (:right :left) 2 3))

; for "out"
(defmethod width  :out [{d :direction}] (case d (:right :left) 3 2))
(defmethod height :out [{d :direction}] (case d (:right :left) 2 3))

; for "inout"
(defmethod width  :inout [{d :direction}] (case d :horizontal 3 2))
(defmethod height :inout [{d :direction}] (case d :horizontal 2 3))

; for "dot"
(defmethod width  :dot [lel] 0)
(defmethod height :dot [lel] 0)

; for "name"
; 0 of size is feasible?
(defmethod width  :name [lel] 0)
(defmethod height :name [lel] 0)

; for "not"
(defmethod x-min  :not [{d :direction [x _] :p}]
  (case d :horizontal      x  :vertical (dec x)))
(defmethod x-max  :not [{d :direction [x _] :p}]
  (case d :horizontal (inc x) :vertical (inc x)))
(defmethod y-min  :not [{d :direction [_ y] :p}]
  (case d :horizontal (dec y) :vertical (dec y)))
(defmethod y-max  :not [{d :direction [_ y] :p}]
  (case d :horizontal (inc y) :vertical      y ))
(defmethod width  :not [{d :direction}]
  (case d :horizontal 1 :vertical 2))
(defmethod height :not [{d :direction}]
  (case d :horizontal 2 :vertical 1))

(defn remove-lel-by-key [lels keys]
  (into {} (remove (fn [[k _]] (keys k)) lels)))

(defn remove-geom-by-key [wires keys]
  (into {} (remove (fn [[k _]] (= (keys k) #{0 1})) wires)))

;--------------------------------------------------
; undo, redo
;--------------------------------------------------
(let [undo-depth 64]
  (defn push-undo [{:keys [lels geoms] :as schem}]
    (-> (dissoc schem :redos)
        (update-in [:undos]
         #(take undo-depth (cons {:lels lels :geoms geoms} %))
         )))
(defn undo-redo [{:keys [lels geoms] :as schem} from to]
  (if (empty? (schem from))
    schem
    (-> schem
        (update-in [to] #(cons {:lels lels :geoms geoms} %))
        (assoc :lels  (:lels  (first (schem from))))
        (assoc :geoms (:geoms (first (schem from))))
        (update-in [from] next))))

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
  (conj {:mode :add :lel (lel-init lel-type) :p []}
        (select-keys schem [:lels :geoms :cursor-pos :cursor-speed
                            :redos :undos])))

(defn catalog-mode [schem]
  {:mode :catalog :catalog-pos [0 0] :revert-schem schem})

;--------------------------------------------------
; move lels and geoms
;--------------------------------------------------

(defn move-lels [lels speed]
  (reduce (fn [lels k] (update-in lels [k :p] #(vec (map + % speed))))
          lels (keys lels)))

(defn move-wire [wire speed points]
  (reduce (fn [wire p-idx] (update-in wire [:p p-idx] #(vec (map + % speed))))
          wire points))

(defn move-geoms [wires speed]
  (into {} (map (fn [[k v]] [k (move-wire v speed #{0 1})])
                wires)))

(defn move-geoms-by-vertices [wires moving-vertices speed]
  (reduce-kv (fn [wires k v]
               (update-in wires [k] move-wire speed v))
             wires moving-vertices))

(defn move-selected [{mv :moving-vertices :as schem} speed]
  (-> schem
      (update-in [:moving-lels ] move-lels  speed)
      (update-in [:moving-geoms] move-geoms speed)
      (update-in [:geoms] move-geoms-by-vertices mv speed)))

(defn move-mode
  [{sl :selected-lels sg :selected-geoms :keys [lels geoms] :as schem}
   & [copy?]]
  (if (and (empty? sl) (empty? sg))
    schem
    (let [{:keys [ml nl]} ; ml: moved lel, nl: not moved lel
          (group-by (fn [[k _]] (if (sl k) :ml :nl)) lels)
          {:keys [mg ng]} ; mg: moved geom, ng: not moved geom
          (group-by (fn [[k _]] (if (= (sg k) #{0 1}) :mg :ng)) geoms)]
      (conj {:mode         (if copy? :copy :move)
             :lels         (if copy? lels (into {} nl))
             :geoms        (if copy? geoms (into {} ng))
             :moving-lels  (into {} ml)
             :moving-geoms (into {} mg)
             :moving-vertices
             (if copy? {} (into {} (remove (fn [[_ v]] (= #{0 1} v)) sg)))
             :revert-schem (if copy? {} schem)}
            (select-keys schem [:cursor-pos :cursor-speed :undos :redos])
            )))))

;--------------------------------------------------
; select
;--------------------------------------------------

(defn find-lel-by-pos [lels [posx posy]]
  (some (fn [[k {[x y] :p}]]
          (when (= [posx posy] [x y]) k))
        lels))

(defn wire-vs-cursor [{[[x0 y0] [x1 y1]] :p} [xc yc]]
  (let [fcomp (fn [qc q0 q1]
                (let [[q0 q1 inv] (if (< q0 q1) [q0 q1 false] [q1 q0 true])]
                  (cond (< qc q0)        nil
                        (< q1 qc)        nil
                        (< (- q1 q0) 4)  (cond (= qc q0) (if inv #{1} #{0})
                                               (= qc q1) (if inv #{0} #{1})
                                               :else     #{0 1})
                        (<= qc (+ q0 1)) (if inv #{1} #{0})
                        (<= (- q1 1) qc) (if inv #{0} #{1})
                        :else            #{0 1}
                        )))]
    (cond (= [xc yc] [x0 y0]) #{0}
          (= [xc yc] [x1 y1]) #{1}
          (= xc x0 x1)        (fcomp yc y0 y1)
          (= yc y0 y1)        (fcomp xc x0 x1)
          :else               nil)))

(defn find-geoms-by-pos [wires pos]
  (reduce-kv (fn [acc k v] (if-let [p (wire-vs-cursor v pos)]
                             (conj acc {k p}) acc))
             {} wires))

; An edge of a line should be selected by surrounding it.
(defn rectangular-select [lels wires [x0 y0] [x1 y1]]
  (let [xmin (min x0 x1) xmax (max x0 x1)
        ymin (min y0 y1) ymax (max y0 y1)
        lels (filter (fn [[k v]]
                       (and (<= xmin (x-min v)) (<= (x-max v) xmax)
                            (<= ymin (y-min v)) (<= (y-max v) ymax)))
                     lels)
        wires (filter (fn [[k {[[x0 y0] [x1 y1]] :p}]]
                        (and (<= xmin (min x0 x1)) (<= (max x0 x1) xmax)
                             (<= ymin (min y0 y1)) (<= (max y0 y1) ymax)))
                      wires)]
    {:lels (set (keys lels))
     :geoms (zipmap (keys wires) (repeat #{0 1}))
     }))

(defn select
  [{:keys [cursor-pos lels geoms rect-p0 selected-lels selected-geoms]
    :as schem}]
  (let [lel-key (find-lel-by-pos lels cursor-pos)
        geom-keys (find-geoms-by-pos geoms cursor-pos)
        rect-keys (if rect-p0
                    (rectangular-select lels geoms rect-p0 cursor-pos)
                    {})
        sl (reduce into [selected-lels (:lels rect-keys)
                         (if lel-key #{lel-key} #{})])
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
   (= (.getCode keyEvent) KeyCode/C) (move-mode schem true)
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

(defn num-p [lel-type]
  (case lel-type
    (:wire :rect) 2
    1))

(defn add-mode-enter [{cursor-pos :cursor-pos :as schem}]
  (let [np (-> schem :lel :type num-p)
        [add-to final-points]
        (cond (= np 1)
              [:lels cursor-pos]
              (<= (dec np) (-> schem :p count))
              [:geoms (conj (:p schem) cursor-pos)]
              :else nil)]
    (println final-points)
    (if final-points
      (-> schem push-undo
          (update-in [add-to] conj
           {(gensym) (assoc (:lel schem) :p final-points)})
          (assoc :p []))
      (update-in schem [:p] conj (:cursor-pos schem))
      )))
 
(defn key-command-add-mode [schem keyEvent]
  (cond
   ; add -> catalog
   (= (.getCode keyEvent) KeyCode/E) (catalog-mode schem)
   ; add -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER) (add-mode-enter schem)
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
  [{[x0 y0] :wire-p0 [x1 y1] :cursor-pos :as schem} keyEvent]
  (cond
   ; wire -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE) (cursor-mode schem)
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER)
   (-> schem push-undo
       (update-in [:geoms] conj {(gensym) {:p [[x0 y0] [x1 y1]]}})
       (assoc :wire-p0 (:cursor-pos schem)))
   :else nil))

(defn key-command-catalog-mode [{[x y] :catalog-pos :as schem} keyEvent]
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

(defn lels-on-line [dir [cx cy] lels]
  (let [on-h-line #(<= (y-min %) cy (y-max %))
        on-v-line #(<= (x-min %) cx (x-max %))]
    (filter (case dir
              :left  #(and (on-h-line %) (< (x-min %) cx))
              :right #(and (on-h-line %) (< cx (x-max %)))
              :up    #(and (on-v-line %) (< (y-min %) cy))
              :down  #(and (on-v-line %) (< cy (y-max %))))
            lels)))

(defn wires-on-line [dir [cx cy] wires]
  (let [on-h-line (fn [{[[_ y0] [_ y1]] :p}] (or (<= y0 cy y1) (<= y1 cy y0)))
        on-v-line (fn [{[[x0 _] [x1 _]] :p}] (or (<= x0 cx x1) (<= x1 cx x0)))]
    (->> (filter (case dir (:left :right) on-h-line on-v-line) wires)
         (filter (case dir
                   :left  (fn [{[[x0 _] [x1 _]] :p}] (< (min x0 x1) cx))
                   :right (fn [{[[x0 _] [x1 _]] :p}] (< cx (max x0 x1)))
                   :up    (fn [{[[_ y0] [_ y1]] :p}] (< (min y0 y1) cy))
                   :down  (fn [{[[_ y0] [_ y1]] :p}] (< cy (max y0 y1)))
                   )))))

(defn coordinates [dir lels wires]
  (let [[lelc0 lelc1] (case dir 0 [x-min x-max] 1 [y-min y-max])]
    (concat (map lelc0 lels) (map lelc1 lels)
            (mapcat (fn [{p :p}] (map #(% dir) p)) wires)
            )))

(defn jump-amount [dir [cx cy :as cursor-pos] lels wires]
  (let [lol (lels-on-line dir cursor-pos (vals lels))
        wol (wires-on-line dir cursor-pos (vals wires))
        [fil pick move-dir] (case dir :left  [#(< % cx) #(apply max %) 0]
                                      :right [#(< cx %) #(apply min %) 0]
                                      :up    [#(< % cy) #(apply max %) 1]
                                      :down  [#(< cy %) #(apply min %) 1])
        filtered (->> (if (and (empty? lol) (empty? wol))
                        [(vals lels) (vals wires)]
                        [lol wol])
                      (apply coordinates move-dir)
                      (filter fil))]
    (if (empty? filtered) 0 (- (pick filtered) (cursor-pos move-dir)))
    ))

(defn move-cursor [schem speed]
  (update-in schem [:cursor-pos] #(vec (map + speed %))))

(defn pane-schem-cursor-speed
  [{cursor-speed :cursor-speed :as schem} keyEvent]
  (let [num ({KeyCode/DIGIT0 0, KeyCode/DIGIT1 1, KeyCode/DIGIT2 2,
              KeyCode/DIGIT3 3, KeyCode/DIGIT4 4, KeyCode/DIGIT5 5,
              KeyCode/DIGIT6 6, KeyCode/DIGIT7 7, KeyCode/DIGIT8 8,
              KeyCode/DIGIT9 9, KeyCode/MINUS :-}
             (.getCode keyEvent))]
    (if (and num (#{:cursor :add :wire :move :copy} (:mode schem)))
      (assoc schem :cursor-speed
             (if (= num :-) 0 (+ (* cursor-speed 10) num)))
      nil)))

(defn pane-schem-cursor-move
  [{:keys [mode cursor-speed cursor-pos lels geoms] :as schem} keyEvent]
  (let [kc (.getCode keyEvent)
        dir (cond (#{KeyCode/LEFT  KeyCode/H} kc) :left
                  (#{KeyCode/RIGHT KeyCode/L} kc) :right
                  (#{KeyCode/UP    KeyCode/K} kc) :up
                  (#{KeyCode/DOWN  KeyCode/J} kc) :down
                  :else                           nil)]
    (when (and dir (#{:cursor :add :wire :move :copy} mode))
      (let [speed (cond (<= cursor-speed 0)
                        (jump-amount dir cursor-pos lels geoms)
                        ('#{:left :up} dir)  (- cursor-speed)
                        :else                cursor-speed)
            speed (case dir (:left :right) [speed 0] [0 speed])
            dir (case dir (:left :right) 0 1)]
        (case mode
          (:cursor :add :wire) (move-cursor schem speed)
          (:move :copy)        (-> (move-cursor schem speed)
                                   (move-selected     speed))
          nil)))))

(defn pane-schem-catalog-move [schem keyEvent]
  (let [kc (.getCode keyEvent)
        [dir f] (cond (#{KeyCode/LEFT  KeyCode/H} kc) [0 dec]
                      (#{KeyCode/RIGHT KeyCode/L} kc) [0 inc]
                      (#{KeyCode/UP    KeyCode/K} kc) [1 dec]
                      (#{KeyCode/DOWN  KeyCode/J} kc) [1 inc]
                      :else                           nil)]
    (if (and dir (= (:mode schem) :catalog))
      (update-in schem [:catalog-pos dir] f))))

(defn pane-schem-key [schem keyEvent]
  (or (pane-schem-cursor-move  schem keyEvent)
      (pane-schem-catalog-move schem keyEvent)
      (pane-schem-cursor-speed schem keyEvent)
      (let [f (key-command (:mode schem))]
        (when f (f schem keyEvent)))))

