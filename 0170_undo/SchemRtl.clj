(ns SchemRtl)

(gen-class
  :name "SchemRtl"
  :main true
  :extends javafx.application.Application)

(import
  '(java.io              File PushbackReader)
  '(javafx.application   Application)
  '(javafx.event         EventHandler)
  '(javafx.geometry      VPos)
  '(javafx.scene         Node Scene)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane Pane VBox)
  '(javafx.scene.paint   Color)
  '(javafx.scene.text    Font Text TextAlignment)
  '(javafx.scene.control Label Button MenuBar Menu MenuItem)
  '(javafx.stage         Stage FileChooser FileChooser$ExtensionFilter))

(require 'clojure.java.io)
(require 'clojure.pprint)
(require 'SchemDialog)       (alias 'sd  'SchemDialog)
(require 'LogicElements)     (alias 'lel 'LogicElements)
(require 'LogicElementsDraw) (alias 'ld  'LogicElementsDraw)

(def pix-per-grid 8.0)
(defn grid2screen [[grid-x grid-y]]
  [(* pix-per-grid grid-x)
   (* pix-per-grid grid-y)])

;--------------------------------------------------
; state
;--------------------------------------------------

(def *label-debug* (Label.))

(def cursor-pos (ref {:x 5 :y 5}))
(def cursor-speed (ref 0))

(def mode (ref {:mode :cursor, :selected-lels #{}, :selected-wires {}}))
; {:mode :cursor, :selected-lels #{}, :selected-wires {},
;  (optional) :rect-p0 {:x ... :y ...}}))
; {:mode :wire, :wire-p0 {:x ... :y ...}}
; {:mode :add , :lel {:type not, :x ...}}
; {:mode            :move,
;  :moving-lels     #{...},
;  :moving-wires    #{...},
;  :moving-vertices {...},
;  :revert-lels     {...},
;  :revert-wires    {...}}
; {:mode :catalog, :catalog-pos {:x ... :y ...}}

(def lels
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               '[{:type :name , :x 5 , :y 20,
                  :string "hoge", :v-align :bottom, :h-align :left}
                 {:type :in   , :x 25, :y 28, :direction :right}
                 {:type :in   , :x 25, :y 22, :direction :right}
                 {:type :and  , :x 32, :y 28, :direction :right,
                  :width 4, :height 4}
                 {:type :or   , :x 40, :y 23, :direction :right,
                  :width 4, :height 4}
                 {:type :in   , :x 25, :y 30, :direction :right}
                 {:type :in   , :x 25, :y 36, :direction :right}
                 {:type :out  , :x 62, :y 26, :direction :right}
                 {:type :in   , :x 25, :y 34, :direction :right}
                 {:type :and  , :x 32, :y 22, :direction :right,
                  :width 4, :height 4}
                 {:type :dot  , :x 30, :y 29}
                 {:type :dff  , :x 55, :y 26, :width 4, :height 5}
                 {:type :mux21, :x 48, :y 24, :direction :right,
                  :width 2, :height 6, :order01 :1->0}
                 ])))

(def wires
  (ref (zipmap (map (fn [_] (gensym)) (repeat '_))
               [{:x0 36, :y0 24, :x1 41, :y1 24}
                {:x0 46, :y0 29, :x1 48, :y1 29}
                {:x0 28, :y0 29, :x1 32, :y1 29}
                {:x0 46, :y0 35, :x1 46, :y1 29}
                {:x0 30, :y0 29, :x1 30, :y1 25}
                {:x0 28, :y0 37, :x1 49, :y1 37}
                {:x0 36, :y0 30, :x1 38, :y1 30}
                {:x0 49, :y0 37, :x1 49, :y1 29}
                {:x0 50, :y0 27, :x1 55, :y1 27}
                {:x0 28, :y0 23, :x1 32, :y1 23}
                {:x0 28, :y0 31, :x1 32, :y1 31}
                {:x0 28, :y0 35, :x1 46, :y1 35}
                {:x0 44, :y0 25, :x1 48, :y1 25}
                {:x0 44, :y0 25, :x1 44, :y1 25}
                {:x0 38, :y0 30, :x1 38, :y1 26}
                {:x0 59, :y0 27, :x1 62, :y1 27}
                {:x0 30, :y0 25, :x1 32, :y1 25}
                {:x0 38, :y0 26, :x1 41, :y1 26}
                ])))

;--------------------------------------------------
; undo, redo
;--------------------------------------------------
(def undos (ref '()))
(def redos (ref '()))
(let [undo-depth 64]
  (defn push-undo [undos lels wires redos]
    (ref-set redos '())
    (alter undos #(take undo-depth (conj % {:lels lels :wires wires})))))
(defn undo-redo [from lels wires to]
  (dosync
    (when-not (empty? @from)
      (alter to conj {:lels @lels :wires @wires})
      (ref-set lels (:lels (first @from)))
      (ref-set wires (:wires (first @from)))
      (alter from rest))))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-mode []
  (case (@mode :mode)
    :cursor  (ld/draw-mode-cursor @mode @cursor-pos
                                  @lels (@mode :selected-lels)
                                  @wires (@mode :selected-wires))
    :move    (ld/draw-mode-move @cursor-pos @lels (@mode :moving-lels)
                                @wires (@mode :moving-wires)
                                (@mode :moving-vertices))
    :copy    (ld/draw-mode-move @cursor-pos @lels (@mode :moving-lels)
                                @wires (@mode :moving-wires) {})
    :add     (ld/draw-mode-add @mode @cursor-pos @lels @wires)
    :wire    (ld/draw-mode-wire @cursor-pos @lels @wires (@mode :wire-p0))
    :catalog (ld/draw-mode-catalog (@mode :catalog-pos))
    ))

;--------------------------------------------------
; move-*
;--------------------------------------------------

(defn move-cursor [dir speed]
  (dosync (alter cursor-pos #(update-in % [dir] (partial + speed)))))

(defn move-selected [dir speed]
  (dosync
    (alter mode update-in [:moving-lels] lel/move-lels dir speed)
    (alter mode update-in [:moving-wires] lel/move-wires dir speed)
    (alter wires lel/move-wires-by-vertices
           (@mode :moving-vertices) dir speed)))

(defn move-catalog [dir speed]
  (dosync (alter mode
           #(update-in % [:catalog-pos dir] (if (neg? speed) dec inc))
           )))

;--------------------------------------------------
; sub functions for key commands
;--------------------------------------------------

;;(defn close-window [frame]
;;  (let [yn (JOptionPane/showConfirmDialog
;;            nil "Do you really want to quit?" "Quit" JOptionPane/YES_NO_OPTION)]
;;    (when (= yn JOptionPane/YES_OPTION)
;;      (.dispose frame))))
;;

;--------------------------------------------------
; dialog box
;--------------------------------------------------

(defn dialog-table [type]
  (case type
    (:in :out)    [[:radio :direction :right :up :left :down]]
    (:inout :not) [[:radio :direction :horizontal :vertical]]
    (:and :or :buf :mux-n)
                  [[:edstr :height read-string]
                   [:edstr :width  read-string]
                   [:radio :direction :right :up :left :down]]
    :name         [[:edstr :string identity]
                   [:radio :h-align :left   :center :right]
                   [:radio :v-align :bottom :center :top  ]]
    :mux21        [[:edstr :height read-string]
                   [:edstr :width  read-string]
                   [:radio :direction :right :up :left :down]
                   [:radio :order01 :0->1 :1->0]]
    (:dff :dffr)  [[:edstr :height read-string]]
                   [:edstr :width  read-string]
    :op           [[:edstr :operator identity   ]
                   [:edstr :height   read-string]
                   [:edstr :width    read-string]]
    nil))

;--------------------------------------------------
; key commands for each mode on schematic panel
;--------------------------------------------------

(defn move-mode [mode lels wires]
  (when-not (and (empty? (mode :selected-lels))
                 (empty? (mode :selected-wires)))
    (let [sl (mode :selected-lels) sw (mode :selected-wires)
          {:keys [ml nl]}
           (group-by #(if (sl (% 0)) :ml :nl) lels)
          {:keys [mw nw]}
           (group-by #(if (= (sw (% 0)) #{:p0 :p1}) :mw :nw)
                     wires)]
      [{:mode            :move
        :moving-lels     (into {} ml)
        :moving-wires    (into {} mw)
        :moving-vertices (into {} (filter (fn [[_ v]] (not= #{:p0 :p1} v))
                                          sw))
        :revert-lels     lels
        :revert-wires    wires}
       nl nw])))

(defn copy-mode [mode lels wires]
  (when-not (and (empty? (mode :selected-lels))
                 (empty? (mode :selected-wires)))
    (let [sl (mode :selected-lels) sw (mode :selected-wires)
          {:keys [ml nl]}
           (group-by #(if (sl (% 0)) :ml :nl) lels)
          {:keys [mw nw]}
           (group-by #(if (= (sw (% 0)) #{:p0 :p1}) :mw :nw)
                     wires)]
      {:mode            :copy
       :moving-lels     (into {} ml)
       :moving-wires    (into {} mw)
       :moving-vertices {}})))

(defn key-command-cursor-mode [keyEvent]
  (cond
   ; cursor -> catalog
   (= (.getCode keyEvent) KeyCode/E)
   (dosync (ref-set mode {:mode :catalog, :catalog-pos {:x 0 :y 0}}))
   ; cursor -> move
   (= (.getCode keyEvent) KeyCode/M)
   (dosync
     (when-let [[new-mode no-move-lels no-move-wires]
                (move-mode @mode @lels @wires)]
       (ref-set mode new-mode)
       (ref-set lels (into {} no-move-lels))
       (ref-set wires (into {} no-move-wires))
       ))
   ; cursor -> copy
   (= (.getCode keyEvent) KeyCode/C)
   (dosync
     (when-let [new-mode (copy-mode @mode @lels @wires)]
       (ref-set mode new-mode)))
   ; cursor -> wire
   (= (.getCode keyEvent) KeyCode/W)
   (dosync (ref-set mode {:mode :wire, :wire-p0 @cursor-pos}))
   ; no mode change
   (= (.getCode keyEvent) KeyCode/R)
   (if (:rect-p0 @mode)
     (dosync (alter mode dissoc :rect-p0))
     (dosync (alter mode assoc :rect-p0 @cursor-pos)))
   (= (.getCode keyEvent) KeyCode/ENTER)
   (let [lel-key (lel/find-lel-by-pos @lels @cursor-pos)
         wire-keys (lel/find-wires-by-pos @wires @cursor-pos)
         rect-keys (if (@mode :rect-p0)
                     (lel/rectangular-select @lels @wires
                       (@mode :rect-p0) @cursor-pos)
                     {})
         sl (reduce into [(:selected-lels @mode) (:lels rect-keys)
                          (if lel-key #{lel-key} #{})])
         sw (reduce (partial merge-with into)
             [(:selected-wires @mode) (:wires rect-keys) wire-keys])]
     (dosync
       (alter mode
              #(-> % (assoc :selected-lels sl)
                     (assoc :selected-wires sw)
                     (dissoc :rect-p0)
                     ))))
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync (alter mode #(-> % (dissoc :rect-p0)
                              (assoc :selected-lels #{})
                              (assoc :selected-wires {})
                              )))
   (= (.getCode keyEvent) KeyCode/X)
   (dosync
     (push-undo undos @lels @wires redos)
     (alter lels lel/remove-lel-by-key (@mode :selected-lels))
     (alter wires lel/remove-wire-by-key (@mode :selected-wires))
     (alter mode #(-> % (assoc :selected-lels #{})
                        (assoc :selected-wires {})
                        )))
   (and (= (.getCode keyEvent) KeyCode/Z)
        (.isControlDown keyEvent))   (undo-redo undos lels wires redos)
   (and (= (.getCode keyEvent) KeyCode/Y)
        (.isControlDown keyEvent))   (undo-redo redos lels wires undos)
   :else :no-consume)) ; cond, defn

; add mode can be merged into move mode
; if continuous addition is not necessary.
(defn key-command-add-mode [keyEvent]
  (cond
   ; add -> catalog
   (= (.getCode keyEvent) KeyCode/E)
   (dosync (ref-set mode {:mode :catalog :catalog-pos {:x 0 :y 0}}))
   ; add -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync (ref-set mode {:mode :cursor,
                          :selected-lels #{}, :selected-wires {}}))
   ; no mode change
   (= (.getCode keyEvent) KeyCode/ENTER)
   (dosync
     (push-undo undos @lels @wires redos)
     (alter lels conj {(gensym) (-> (:lel @mode)
                                    (assoc :x (:x @cursor-pos))
                                    (assoc :y (:y @cursor-pos))
                                    )}))
   :else :no-consume))

(defn key-command-move-mode [keyEvent]
  (cond
   ; move -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync
     (ref-set lels (:revert-lels @mode))
     (ref-set wires (:revert-wires @mode))
     (ref-set mode {:mode :cursor,
                    :selected-lels #{}, :selected-wires {}}))
   (= (.getCode keyEvent) KeyCode/ENTER)
   (dosync
     (push-undo undos @lels @wires redos)
     (alter lels conj (:moving-lels @mode))
     (alter wires conj (:moving-wires @mode))
     (ref-set mode {:mode :cursor,
                    :selected-lels #{}, :selected-wires {}}))
   :else :no-consume))

(defn key-command-copy-mode [keyEvent]
  (cond
   ; move -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync
     (ref-set mode {:mode :cursor,
                    :selected-lels #{}, :selected-wires {}}))
   (= (.getCode keyEvent) KeyCode/ENTER)
   (dosync
     (push-undo undos @lels @wires redos)
     (alter lels into (map (fn [[k v]] [(gensym) v])
                           (:moving-lels @mode)))
     (alter wires into (map (fn [[k v]] [(gensym) v])
                            (:moving-wires @mode))))
   :else :no-consume))

(defn key-command-wire-mode [keyEvent]
  (cond
   ; wire -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync
     (ref-set mode {:mode :cursor,
                    :selected-lels #{}, :selected-wires {}}))
   (= (.getCode keyEvent) KeyCode/ENTER)
   (dosync
     (push-undo undos @lels @wires redos)
     (alter wires conj
            {(gensym) {:x0 (get-in @mode [:wire-p0 :x])
                       :y0 (get-in @mode [:wire-p0 :y])
                       :x1 (@cursor-pos :x)
                       :y1 (@cursor-pos :y)}})
     (alter mode assoc :wire-p0 @cursor-pos))
   :else :no-consume))

(defn key-command-catalog-mode [keyEvent]
  (cond
   ; catalog -> add
   (= (.getCode keyEvent) KeyCode/ENTER)
   (when-let [type (get-in ld/catalog-table
                           (map #(get-in @mode [:catalog-pos %]) [:y :x]))]
     (dosync (ref-set mode {:mode :add :lel (lel/lel-init type)})))
   ; catalog -> cursor
   (= (.getCode keyEvent) KeyCode/ESCAPE)
   (dosync (ref-set mode {:mode :cursor,
                          :selected-lels #{}, :selected-wires {}}))
   :else :no-consume))

(def key-command
  {:cursor  key-command-cursor-mode
   :add     key-command-add-mode
   :move    key-command-move-mode
   :copy    key-command-copy-mode
   :wire    key-command-wire-mode
   :catalog key-command-catalog-mode
   })

(defn state-text []
  (reduce #(str %1 "\n" %2)
          [(reduce #(dissoc %1 %2) @mode [:revert-lels :revert-wires])
           (str @cursor-pos " " @cursor-speed " "
                (count @redos) " " (count @undos))
           @lels @wires]))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(defn pane-schem-cursor-speed [keyEvent]
  (let [num ({KeyCode/DIGIT0 0, KeyCode/DIGIT1 1, KeyCode/DIGIT2 2,
              KeyCode/DIGIT3 3, KeyCode/DIGIT4 4, KeyCode/DIGIT5 5,
              KeyCode/DIGIT6 6, KeyCode/DIGIT7 7, KeyCode/DIGIT8 8,
              KeyCode/DIGIT9 9, KeyCode/MINUS :-}
             (.getCode keyEvent))]
    (when (and num (#{:cursor :add :wire :move :copy} (:mode @mode)))
      (dosync (ref-set cursor-speed
               (if (= num :-) 0 (+ (* @cursor-speed 10) num))))
      (.consume keyEvent)
      true)))

(defn pane-schem-cursor-move [keyEvent pane]
  (let [kc (.getCode keyEvent)
        dir (cond (#{KeyCode/LEFT  KeyCode/H} kc) :left
                  (#{KeyCode/RIGHT KeyCode/L} kc) :right
                  (#{KeyCode/UP    KeyCode/K} kc) :up
                  (#{KeyCode/DOWN  KeyCode/J} kc) :down
                  :else                           nil)
        speed (cond (not dir)            1
                    (<= @cursor-speed 0)
                      (lel/jump-amount dir @cursor-pos @lels @wires)
                    ('#{:left :up} dir)  (- @cursor-speed)
                    :else                @cursor-speed)
        op (case (:mode @mode)
             (:cursor :add :wire) #(move-cursor % speed)
             (:move :copy)        #(do (move-cursor   % speed)
                                       (move-selected % speed))
             :catalog             #(move-catalog % (case dir (:left :up) -1 1))
             nil)]
    (when (and dir op)
      (op (case dir (:left :right) :x :y))
      (.consume keyEvent)
      (.setAll (.getChildren pane) (draw-mode))
      (.consume keyEvent)
      true)))

(defn pane-schem-revert [f-set-to-parent pane]
  (f-set-to-parent pane)
  (.setAll (.getChildren pane) (draw-mode))
  (.setFocusTraversable pane true)
  (.requestFocus pane))

(defn pane-schem-goto-dialog [keyEvent pane f-set-to-parent]
  (when-let [[lel lel-update-fn]
              (cond (and (= KeyCode/V (.getCode keyEvent))
                         (= (:mode @mode) :cursor))
                    (when-let [lel-key
                               (lel/find-lel-by-pos @lels @cursor-pos)]
                      [(@lels lel-key)
                       #(dosync
                          (push-undo undos @lels @wires redos)
                          (alter lels assoc lel-key %))])

                    (and (= KeyCode/V (.getCode keyEvent))
                         (= (:mode @mode) :add))
                    [(@mode :lel)
                     #(dosync (alter mode assoc :lel %))])]
    (when-let [dt (dialog-table (:type lel))]
      (.setFocusTraversable pane false)
      (let [borderpane (BorderPane.)
            dialog (sd/pane-dialog #(.setRight borderpane %)
                   #(pane-schem-revert f-set-to-parent pane)
                   dt lel lel-update-fn)]
        (.setCenter borderpane pane)
        (f-set-to-parent borderpane)
        (.setFocusTraversable dialog true)
        (.requestFocus dialog)
        (.consume keyEvent)
        true))))

(defn pane-schem-key [f-set-to-parent pane]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (or (pane-schem-goto-dialog  keyEvent pane f-set-to-parent)
          (pane-schem-cursor-move  keyEvent pane)
          (pane-schem-cursor-speed keyEvent)
          (let [f (key-command (:mode @mode))]
            (when (and f (not= (f keyEvent) :no-consume))
              (.setAll (.getChildren pane) (draw-mode))
              (.consume keyEvent))))
      (.setText *label-debug* (state-text))
      )))

(defn pane-schem [f-set-to-parent]
  (let [pane (Pane.)]
    (.setOnKeyPressed pane (pane-schem-key f-set-to-parent pane))
    (.setFocusTraversable pane true)
    (.setAll (.getChildren pane) (draw-mode))
    (f-set-to-parent pane)
    pane))

;--------------------------------------------------
; Menus
;--------------------------------------------------
(defn my-file-chooser [main-stage title default-dir is-save]
  (let [fileChooser (FileChooser.)]
    (.setTitle fileChooser title)
    (when (and default-dir (.exists default-dir) (.isDirectory default-dir))
      (.setInitialDirectory fileChooser default-dir))
    (.. fileChooser getExtensionFilters
     (addAll
      (into-array FileChooser$ExtensionFilter
       [(FileChooser$ExtensionFilter. "RTL Schematica (*.rtc)"
         (into-array String ["*.rtc"] ))
        (FileChooser$ExtensionFilter. "All Files (*.*)"
         (into-array String ["*.*"]))])))
    (if is-save
      (.showSaveDialog fileChooser main-stage)
      (.showOpenDialog fileChooser main-stage))))

(let [prev-path (atom nil)]
  (defn action-open [main-stage]
    (proxy [EventHandler] []
      (handle [_]
        (let [file (my-file-chooser main-stage "Open File" @prev-path false)
              rd (when file (PushbackReader. (clojure.java.io/reader file)))]
          (when rd
            (dosync (doseq [r [lels wires]] (ref-set r (read rd))))
            (reset! prev-path (.getParentFile file))
            (.close rd)
            ))))))

(let [prev-path (atom nil)]
  (defn action-save-as [main-stage]
    (proxy [EventHandler] []
      (handle [_]
        (let [file (my-file-chooser main-stage "Save File As"
                                    @prev-path true)
              wr (when file (clojure.java.io/writer file))]
          (when wr
            (doseq [x [@lels @wires]]
              (clojure.pprint/pprint x wr))
            (reset! prev-path (.getParentFile file))
            (.close wr)
            ))))))

(defn action-exit []
  (proxy [EventHandler] []
    (handle [_]
      (System/exit 0))))

(defn pane-about [f-revert]
  (let [label (Label. (str "RTL Schematica ver. 0.0.0\n"
                           "Authored by Tackya Yammouch"))
        ok-button (Button. "OK")
        pane (VBox.)]
    (.setDefaultButton ok-button true)
    (.setOnAction ok-button
     (proxy [EventHandler] []
       (handle [_] (f-revert))))
    (.. pane getChildren
     (setAll
      (into-array Node [label ok-button])))
    pane))

(defn action-about [f-set-to-parent f-revert]
  (proxy [EventHandler] []
    (handle [_]
      (f-set-to-parent (pane-about f-revert))
      )))

(defn menu-top [main-stage f-set-to-parent f-revert]
  (let [menu (MenuBar.)
        file (Menu. "File")
        open (MenuItem. "Open")
        save-as (MenuItem. "Save As")
        exit (MenuItem. "Exit")
        help (Menu. "help")
        about (MenuItem. "About")]
    (.setOnAction open (action-open main-stage))
    (.setOnAction save-as (action-save-as main-stage))
    (.setOnAction exit (action-exit))
    (.setOnAction about (action-about f-set-to-parent f-revert))
    (doseq [x [open save-as exit]]
      (.. file getItems (add x)))
    (.. help getItems (add about))
    (doseq [x [file help]]
      (.. menu getMenus (add x)))
    (.setFocusTraversable menu true)
    menu))

;--------------------------------------------------
; JavaFX main routine
;--------------------------------------------------
(defn -start [self stage]
  (let [topgroup (BorderPane.)
        pane (pane-schem #(.setCenter topgroup %))
        menu (menu-top stage
                       #(.setCenter topgroup %)
                       #(.setCenter topgroup pane))]
    (.setWrapText *label-debug* true)
    (.setTop topgroup menu)
    (.setBottom topgroup *label-debug*)
    (.setText *label-debug* (state-text))
    (doto stage
      (.setWidth 1024) (.setHeight 768)
      (.setScene (Scene. topgroup))
      (.setTitle "Shows Some Gates")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "SchemRtl")
                      (into-array String [])))

