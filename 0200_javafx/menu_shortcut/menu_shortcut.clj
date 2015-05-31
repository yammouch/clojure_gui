(ns menu_shortcut)

(gen-class
  :name "menu_shortcut"
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

;--------------------------------------------------
; Menus
;--------------------------------------------------
(defn action-open []
  (proxy [EventHandler] []
    (handle [_]
      (println "Open"))))

(defn action-save-as []
  (proxy [EventHandler] []
    (handle [_]
      (println "Save As"))))

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
    (.setOnAction open (action-open))
    (.setOnAction save-as (action-save-as))
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
        pane (Pane.)
        menu (menu-top stage
                       #(.setCenter topgroup %)
                       #(.setCenter topgroup pane))]
    (.setTop topgroup menu)
    (doto stage
      (.setWidth 640) (.setHeight 480)
      (.setScene (Scene. topgroup))
      (.setTitle "Menu")
      (.show))
    (.requestFocus pane)))

(defn -main [& args]
  (Application/launch (Class/forName "menu_shortcut")
                      (into-array String [])))

