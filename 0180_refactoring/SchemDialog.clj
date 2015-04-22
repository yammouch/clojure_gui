(ns SchemDialog)

(gen-class
  :name "SchemDialog")

(import
  '(javafx.event         EventHandler)
  '(javafx.scene.input   KeyCode KeyEvent)
  '(javafx.scene.layout  BorderPane VBox FlowPane)
  '(javafx.scene.paint   Color)
  '(javafx.scene.shape   Polygon)
  '(javafx.scene.control Label TextArea RadioButton ToggleGroup Button))

;--------------------------------------------------
; splits panes
;--------------------------------------------------
(defn split-pane [self set-to-self-pos create-container child]
  (set-to-self-pos (create-container self child))
  (.setFocusTraversable self false)
  (.setFocusTraversable child true)
  (.requestFocus child))

(defn revert-from-split [self set-to-self-pos]
  (set-to-self-pos self)
  (.setFocusTraversable self true)
  (.requestFocus self))

;--------------------------------------------------
; text area
;--------------------------------------------------

(defn pane-text-key-dialog [revert textarea label]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (let [op (cond (and (= (.getCode keyEvent) KeyCode/ENTER)
                          (.isShiftDown keyEvent))
                     :ok
                     (= (.getCode keyEvent) KeyCode/ESCAPE)
                     :cancel
                     :else nil)]
        (when op
          (when (= op :ok) (.setText label (.getText textarea)))
          (.consume keyEvent)
          (revert)
          )))))

(defn pane-text [revert label]
  (let [textarea (TextArea. (.getText label))]
    (.setOnKeyPressed textarea (pane-text-key-dialog revert textarea label))
    textarea))

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

(defn selected-row [rows] ; cursored-row?
  (first (filter #(not= (.getFill (:cursor %)) Color/TRANSPARENT)
                 rows)))

(defn suck-from-dialog [rows]
  (map (fn [r]
         [(:label r)
          (case (:type r)
            :edstr ((:cast r) (.getText (:str r)))
            :radio (keyword (.. (:togglegroup r)
                                getSelectedToggle getText)))])
       rows))

(defn pane-dialog-key [set-to-self-pos revert self rows lel f-retval]
  (proxy [EventHandler] []
    (handle [keyEvent]
      (cond
       (= (.getCode keyEvent) KeyCode/ENTER)
       (do (f-retval (into lel (suck-from-dialog rows)))
           (revert))
       (= (.getCode keyEvent) KeyCode/ESCAPE)
       (revert)
       (= (.getCode keyEvent) KeyCode/SPACE)
       (let [row (selected-row rows)] (when (= (:type row) :edstr)
         (split-pane self set-to-self-pos #(BorderPane. %1 nil %2 nil nil)
          (pane-text #(revert-from-split self set-to-self-pos) (:str row))
          )))
       (#{KeyCode/J KeyCode/K} (.getCode keyEvent))
       (pane-dialog-cursor-move (map #(:cursor %) rows)
        (if (= (.getCode keyEvent) KeyCode/J) :down :up))
       (#{KeyCode/H KeyCode/L} (.getCode keyEvent))
       (let [row (selected-row rows)] (when (= (:type row) :radio)
         (pane-dialog-radio-button-move (:togglegroup row)
          (if (= (.getCode keyEvent) KeyCode/H) :left :right)
          )))))))

(defn pane-dialog [set-to-self-pos revert table lel f-retval]
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
     (pane-dialog-key set-to-self-pos revert pane rows lel f-retval))
    pane))
