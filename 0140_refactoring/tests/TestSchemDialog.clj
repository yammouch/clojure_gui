;(ns tests.TestSchemDialog)
(ns tests.TestSchemDialog)

(gen-class
  :name "tests.TestSchemDialog"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application   Application)
  '(javafx.event         EventHandler)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane)
  '(javafx.scene.control Label)
  '(javafx.scene         Scene)
  '(javafx.stage         Stage))

(require 'SchemDialog)
(alias 'sd 'SchemDialog)
(require 'clojure.pprint)

(defn label-revert [f-set-to-parent label]
  (f-set-to-parent label)
  (.setFocusTraversable label true)
  (.requestFocus label))

(defn label-key [f-set-to-parent label]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (cond (= (.getCode keyEvent) KeyCode/D)
            (let [lel {:radio1 :FM-Iruka :radio2 :NHK2 :frequency 88.8}
                  dialog (sd/pane-dialog
                          f-set-to-parent
                          #(label-revert f-set-to-parent label)
                          [[:radio :radio1 :NHK :Air-G :Northwave :FM-Iruka]
                           [:radio :radio2 :NHK1 :NHK2 :HBC :STV]
                           [:edstr :frequency read-string]]
                          lel clojure.pprint/pprint)]
              (.setFocusTraversable label false)
              (f-set-to-parent dialog)
              (.setFocusTraversable dialog true)
              (.requestFocus dialog)
              (.consume keyEvent))

           :else nil ; do nothing
           ))))

(defn -start [self stage]
  (let [toppane (BorderPane.)
        label (Label. "Press D key to open dialog")]
    (.setOnKeyPressed label
                      (label-key #(.setCenter toppane %) label))
    (.setFocusTraversable label true)
    (doto stage
      (.setWidth 640) (.setHeight 480)
      (.setScene (Scene. toppane))
      (.setTitle "Schematic Dialog Box Unit Test")
      (.show))
    (.requestFocus label)))

(defn -main [& args]
  (Application/launch (Class/forName "tests.TestSchemDialog")
                      (into-array String [])))
