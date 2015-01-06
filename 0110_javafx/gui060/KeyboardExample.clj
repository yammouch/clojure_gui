(ns KeyboardExample)

(gen-class
  :name "KeyboardExample
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application    Application)
  '(javafx.beans.binding  Bindings)
  '(javafx.beans.property BooleanProperty)
  '(javafx.beans.property SimpleBooleanProperty)
  '(javafx.event          EventHandler)
  '(javafx.geometry       Insets)
  '(javafx.scene          Group Node Parent Scene)
  '(javafx.scene.input    KeyCode KeyEvent)
  '(javafx.scene.layout   HBox StackPane)
  '(javafx.scene.paint    Color)
  '(javafx.scene.shape    Rectangle)
  '(javafx.scene.text     Font FontWeight Text)
  '(javafx.stage          Stage))

;(defn -start [self stage] ...)

(defn key-new [keyCode pressedProperty]
  (let [keyEventHandler (proxy [EventHandler] []
                          (handle [keyEvent]
                            (when (= (.getCode keyEvent) KeyCode/ENTER)
                              (.set pressedProperty
                                    (= (.getEventType keyEvent)
                                       KeyEvent/KEY_PRESSED))
                              (.consume keyEvent))))
        keyBackgroung (Rectangle. 50 50)
        keyLabel (Text. (.getName keyCode))
        keyNode (StackPane.)]
    (.. keyBackground fillProperty
        (bind (.. (Bindings/when pressedProperty)
                  (then Color/RED)
                  (otherwise (.. (Bindings/when (.focusedProperty keyNode))
                                 (then Color/LIGHTGRAY)
                                 (otherwise Color/WHITE)
                                 )))))
    (doto keyBackground
      (.setStroke      Color/BLACK)
      (.setStrokeWidth 2)
      (.setArcWidth    12)
      (.setArcHeight   12))
    (.setFont keyLabel (Font/font "Arial" FontWeight/BOLD 20))
    (doto keyNode
      (.setFocusTraversable true)
      (.setOnKeyPressed     keyEventHandler)
      (.setOnKeyReleased    keyEventHandler))
    (doseq [x [keyBackground keyLabel]]
      (.. keyNode getChildren (add x))
      )))

(defn get-next-node [parent node]
  (loop [childIterator (.. parent getChildrenUnmodifiable iterator)]
    (cond (not (.hasNext childIterator)) nil
          (= (.next childIterator) node)
            (if (.hasNext childIterator) (.next childIterator) nil)
          :else (recur childIterator))))

(defn get-previous-node [parent node]
  (loop [childIterator (.. parent getChildrenUnmodifiable iterator)
         lastNode nil]
    (if (.hasNext chileIterator)
      (let [currentNode (.next childIterator)]
        (if (= currentNode node)
          lastNode
          (recur childIterator currentNode)))
      nil)))

;keys [[KeyCode/A (SimpleBooleanProperty.)]

(defn keyboard-new [& keys]
  (let [pressedProperties (map (fn [_] (SimpleBooleanProperty.)) keys)
        keys-lookup (zipmap keys pressedProperties)
        keyboardNode (Hbox. 6)
        keyEventHandler
         (proxy [EventHandler] []
           (handle [keyEvent]
             (let [pressedProperty (keys-lookup (.getCode keyEvent))]
               (when pressedProperty
                 (.set pressedProperty (= (.getEventType keyEvent)
                                          KeyEvent/KEY_PRESSED))
                 (.consume keyEvent)))))]
    (doseq [[key pressedProperty] (map list keys pressedProperties)]
      (.. keyboardNode getChildren
          (add (key-new key pressedProperty)))
    (doto keyboardNode
      (.setPadding (Insets. 6))
      (.setOnKeyPressed  keyEventHandler)
      (.setOnKeyReleased keyEventHandler)
      ( .addEventHandler KeyEvent/KEY_PRESSED
        (proxy [EventHandler] []
          (handle [keyEvent]
            (let [nextFocusedNode
                    (case (.getCode keyEvent)
                      KeyEvent/LEFT  ( get-previous-node
                                       keyboardNode (.getTarget keyEvent))
                      KeyEvent/RIGHT ( get-next-node
                                       keyboardNode (.getTarget keyEvent))
                      nil)]
              (when nextFocusedNode
                (.requestFocus nextFocusedNode)
                ))))))
  keyboardNode))

(defn -start [self stage]
  (doto stage
    ( .setScene
      ( Scene.
        ( Group. (keyboard-new KeyCode/A KeyCode/S KeyCode/D KeyCode/F))))
    (.setTitle "Keyboard Example")
    (.show)))

(defn -main [& args]
  (Application/launch (Class/forName "KeyboardExample")
                      (into-array ^String [])))
