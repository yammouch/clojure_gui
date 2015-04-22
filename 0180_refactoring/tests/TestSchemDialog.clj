(ns tests.TestSchemDialog)

(gen-class
  :name "tests.TestSchemDialog"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application   Application Platform)
  '(javafx.event         Event EventHandler)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane)
  '(javafx.scene.control Label)
  '(javafx.scene         Scene)
  '(javafx.stage         Stage))

(require 'SchemDialog)
(alias 'sd 'SchemDialog)
(require 'clojure.pprint)

(defn auto-key-events [stage]
  (Thread/sleep 500)
  (let [fn-label #(.. stage getScene getRoot)
        fn-dialog #(.. stage getScene getRoot getCenter)
        fn-textarea #(.. stage getScene getRoot getCenter getBottom)]
    (doseq [[fn-target e]
            (map #(vector (% 0)
                          (KeyEvent. KeyEvent/KEY_PRESSED (% 1) (% 1) (% 2)
                           (% 3) false false false)) ; shift ctrl alt meta
                 [[fn-label    "d"  KeyCode/D     false]
                  [fn-dialog   "h"  KeyCode/H     false]
                  [fn-dialog   "j"  KeyCode/J     false]
                  [fn-dialog   "l"  KeyCode/L     false]
                  [fn-dialog   "l"  KeyCode/L     false]
                  [fn-dialog   "j"  KeyCode/J     false]
                  [fn-dialog   " "  KeyCode/SPACE false]
                  [fn-textarea "f"  KeyCode/F     false]
                  [fn-textarea "o"  KeyCode/O     false]
                  [fn-textarea "o"  KeyCode/O     false]
                  [fn-textarea "\n" KeyCode/ENTER true ]
                  [fn-dialog   "\n" KeyCode/ENTER false]])]
      (Thread/sleep 500)
      (print "Fires ") (println (.toString e))
      (Platform/runLater #(.fireEvent (fn-target) e))
      )))

(defn label-key [set-to-self-pos label stage]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (print "[Label] received ") (println (.toString keyEvent))
      (cond (= (.getCode keyEvent) KeyCode/D)
            (let [lel {:radio1 :FM-Iruka :radio2 :NHK2 :frequency 88.8}
                  borderpane (BorderPane.)
                  dialog (sd/pane-dialog
                          #(.setCenter borderpane %)
                          #(sd/revert-from-split label set-to-self-pos)
                          [[:radio :radio1 :NHK :Air-G :Northwave :FM-Iruka]
                           [:radio :radio2 :NHK1 :NHK2 :HBC :STV]
                           [:edstr :frequency read-string]]
                          lel clojure.pprint/pprint)]
              (sd/split-pane label set-to-self-pos
               #(doto borderpane (.setCenter %2) (.setTop %1))
               dialog)
              (.consume keyEvent))
            (= (.getCode keyEvent) KeyCode/A)
            (.start (Thread. #(auto-key-events stage)))
            :else nil ; do nothing
            ))))

(defn stage-key []
  (proxy [EventHandler] []
    (handle [keyEvent]
      (print "[Stage] received ") (println (.toString keyEvent))
      )))

(defn -start [_ stage]
  (let [label (Label. "Press D key to open dialog")
        scene (Scene. label)]
    (.setOnKeyPressed label (label-key #(.setRoot scene %) label stage))
    (.setFocusTraversable label true)
    (doto stage
      (.setWidth 640) (.setHeight 480)
      (.setScene scene)
      (.setTitle "Schematic Dialog Box Unit Test")
      (.show))
    (.requestFocus label)
    (.addEventHandler stage KeyEvent/KEY_PRESSED (stage-key))
    (.start (Thread. #(auto-key-events stage)))))

(defn -main [& args]
  (Application/launch (Class/forName "tests.TestSchemDialog")
                      (into-array String [])))
