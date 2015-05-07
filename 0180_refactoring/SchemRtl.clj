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

;--------------------------------------------------
; state
;--------------------------------------------------

(def *label-debug* (Label.))

(def schem (ref (conj (read-string (slurp "sample_design00.rtc"))
                      {:mode :cursor :selected-lels #{} :selected-geoms {}
                       :cursor-pos [5 5] :cursor-speed 1
                       :redos '() :undos '()})))

;--------------------------------------------------
; draw-*
;--------------------------------------------------

(defn draw-mode [schem]
  (case (:mode schem)
    :cursor  (ld/draw-mode-cursor  schem)
    :move    (ld/draw-mode-move    schem)
    :copy    (ld/draw-mode-move    schem)
    :add     (ld/draw-mode-add     schem)
    :catalog (ld/draw-mode-catalog schem)
    ))

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
    :dff          [[:edstr :height read-string]
                   [:edstr :width  read-string]
                   [:radio :async-reset :true :false]]
    :op           [[:edstr :operator identity   ]
                   [:edstr :height   read-string]
                   [:edstr :width    read-string]]
    nil))

(defn state-text [{:keys [cursor-pos cursor-speed redos undos lels geoms]
                   :as schem}]
  (apply str (interpose "\n"
              [(reduce #(dissoc %1 %2) schem
                [:revert-schem :undos :redos :cursor-pos :cursor-speed])
               (apply str (interpose " "
                [cursor-pos cursor-speed (count redos) (count undos)]
                ))])))

;--------------------------------------------------
; schematic pane
;--------------------------------------------------

(defn pane-schem-goto-dialog [keyEvent pane f-set-to-parent]
  (when-let [[lel lel-update-fn]
             (when (= KeyCode/V (.getCode keyEvent))
               (case (:mode @schem)
                 :cursor
                 (when-let [lel-key
                            (lel/find-lel-by-pos
                             (:lels @schem) (:cursor-pos @schem))]
                   [(get-in @schem [:lels lel-key])
                    #(dosync (ref-set schem
                                      (-> @schem lel/push-undo
                                          (assoc-in [:lels lel-key] %))))])
                 :add
                 [(@schem :lel)
                  #(dosync (alter schem assoc :lel %))]))]
    (when-let [dt (dialog-table (:type lel))]
      (.setFocusTraversable pane false)
      (let [borderpane (BorderPane.)
            dialog (sd/pane-dialog #(.setRight borderpane %)
                   #(do (sd/revert-from-split pane f-set-to-parent)
                        (.setAll (.getChildren pane) (ld/draw-mode @schem)))
                   dt lel lel-update-fn)]
        (sd/split-pane pane f-set-to-parent
         #(doto borderpane (.setCenter %1) (.setRight %2)) dialog)
        (.consume keyEvent)
        true))))

(defn pane-schem-key [f-set-to-parent pane]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (cond (pane-schem-goto-dialog keyEvent pane f-set-to-parent)
            (.consume keyEvent)
            (when-let [schem-updated (lel/pane-schem-key @schem keyEvent)]
              (dosync
                (ref-set schem schem-updated)
                (.setAll (.getChildren pane) (ld/draw-mode @schem))
                (.setText *label-debug* (state-text @schem)))
              (.consume keyEvent)
              true)
            :done))))

(defn pane-schem [f-set-to-parent]
  (let [pane (Pane.)]
    (.setOnKeyPressed pane (pane-schem-key f-set-to-parent pane))
    (.setFocusTraversable pane true)
    (.setAll (.getChildren pane) (ld/draw-mode @schem))
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
            (dosync
              (ref-set schem
                       (conj (read rd)
                        {:mode :cursor :selected-lels #{} :selected-geoms {}
                         :cursor-pos [5 5] :cursor-speed 1
                         :redos '() :undos '()})))
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
            (clojure.pprint/pprint (select-keys @schem [:lels :geoms]) wr)
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
    (.setText *label-debug* (state-text @schem))
    (doto stage
      (.setWidth 1024) (.setHeight 768)
      (.setScene (Scene. topgroup))
      (.setTitle "Shows Some Gates")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "SchemRtl")
                      (into-array String [])))

