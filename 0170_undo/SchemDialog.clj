(ns SchemDialog)

(gen-class
  :name "SchemDialog")

(import
  '(javafx.event         EventHandler)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane VBox FlowPane)
  '(javafx.scene.paint   Color)
  '(javafx.scene.shape   Polygon)
  '(javafx.scene.control Label TextField RadioButton ToggleGroup Button))

;--------------------------------------------------
; text field
;--------------------------------------------------

(defn pane-text [f-set-to-parent str]
  (let [textfield (TextField. str)]
    (.setFocusTraversable textfield true)
    (f-set-to-parent textfield)
    textfield))

;--------------------------------------------------
; dialog box
;--------------------------------------------------

(defn prev-next [f list]
  (loop [prev nil l list]
    (cond (empty? l) [prev prev]
          (f (first l))
            [(if prev prev (first l))
             (if (empty? (next l)) (first l) (fnext l))]
          :else (recur (first l) (next l))
          )))

(defn pane-dialog-cursor-move [cursors dir]
  (let [[prv nxt] (prev-next #(not= (.getFill %) Color/TRANSPARENT) cursors)
        target (case dir :up prv, :down nxt)]
    (doseq [c cursors]
      (.setFill c (if (= c target) Color/BLACK Color/TRANSPARENT))
      )))

(defn pane-dialog-radio-button-move [toggleGroup dir]
  (let [toggles (.getToggles toggleGroup)
        [prv nxt] (prev-next #(.isSelected %) toggles)]
    (.selectToggle toggleGroup (case dir :left prv, :right nxt))))

(defn pane-text-key-dialog [f-revert textfield label-on-dialog]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (when (#{KeyCode/ENTER KeyCode/ESCAPE} (.getCode keyEvent))
        (when (= (.getCode keyEvent) KeyCode/ENTER)
          (.setText label-on-dialog (.getText textfield)))
        (.consume keyEvent)
        (f-revert)
        ))))

(defn pane-dialog-revert [f-set-to-parent pane]
  ;(.setText *label-debug* (state-text))
  (f-set-to-parent pane)
  (.setFocusTraversable pane true)
  (.requestFocus pane))

(defn pane-dialog-key [f-set-to-parent f-revert pane rows lel f-retval]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [kc (.getCode keyEvent)]
        (cond (= KeyCode/ENTER kc)
                (do
                  (f-retval
                    (into lel
                     (map (fn [r]
                            [(:label r)
                             (case (:type r)
                               :edstr ((:cast r) (.getText (:str r)))
                               :radio (keyword
                                       (.. (:togglegroup r)
                                           getSelectedToggle getText)))])
                          rows)))
                  (f-revert))
              (= KeyCode/ESCAPE kc)
                (f-revert)
              (= KeyCode/SPACE kc)
                (let [row (first (filter #(not= (.getFill (:cursor %))
                                                Color/TRANSPARENT)
                                         rows))]
                  (when (= (:type row) :edstr)
                    (let [borderpane (BorderPane.)
                          textfield
                            (pane-text #(.setBottom borderpane %)
                                       (.getText (:str row)))]
                      (.setOnKeyPressed textfield
                       (pane-text-key-dialog
                        #(pane-dialog-revert f-set-to-parent pane)
                        textfield (:str row)))
                      (.setFocusTraversable pane false)
                      (.setCenter borderpane pane)
                      (f-set-to-parent borderpane)
                      (.setFocusTraversable textfield true)
                      (.requestFocus textfield))))
              (#{KeyCode/J KeyCode/K} kc)
                (pane-dialog-cursor-move (map #(:cursor %) rows)
                 (if (= kc KeyCode/J) :down :up))
              (#{KeyCode/H KeyCode/L} kc)
                (let [row (first (filter
                                  #(not= (.getFill (:cursor %))
                                         Color/TRANSPARENT)
                                  rows))]
                  (when (= (:type row) :radio)
                    (pane-dialog-radio-button-move
                     (:togglegroup row)
                     (if (= kc KeyCode/H) :left :right))))
              )))))

(defn pane-dialog [f-set-to-parent f-revert table lel f-retval]
  (let [pane (VBox.)
        rows (map (fn [x]
                    (conj
                     {:type (x 0)
                      :cursor (Polygon.
                               (double-array [0.0 0.0 10.0 5.0 0.0 10.0]))
                      :flowpane (FlowPane.)
                      :label (x 1)}
                     (case (first x)
                      :edstr {:str (Label. (str (lel (x 1))))
                              :cast (x 2)}
                      :radio {:togglegroup (ToggleGroup.)
                              :buttons
                                (map (fn [y] (RadioButton. (name y)))
                                     (drop 2 x)
                                     )})))
                  table)]
    (.setSpacing pane 8.0)
    (doseq [r rows]
      (.setFill (:cursor r) Color/TRANSPARENT)
      (.setVgap (:flowpane r) 12.0)
      (.setHgap (:flowpane r)  8.0)
      (.. (:flowpane r) getChildren (add (:cursor r)))
      (.. (:flowpane r) getChildren (add (Label. (name (:label r)))))
      (case (:type r)
        :edstr (.. (:flowpane r) getChildren (add (:str r)))
        :radio (doseq [button (:buttons r)]
                 (.setToggleGroup button (:togglegroup r))
                 (when (= (lel (:label r)) (keyword (.getText button)))
                   (.selectToggle (:togglegroup r) button))
                 (.. (:flowpane r) getChildren (add button))))
      (.. pane getChildren (add (:flowpane r))))
    (.setFill (:cursor (first rows)) Color/BLACK)
    (.setOnKeyPressed pane
                      (pane-dialog-key f-set-to-parent f-revert pane
                                       rows lel f-retval))
    (f-set-to-parent pane)
    pane))
