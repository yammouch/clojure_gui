(ns tests.TestLogicElementsDraw)

(gen-class
  :name "tests.TestLogicElementsDraw"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application  Application)
  '(javafx.scene.layout GridPane Pane)
  '(javafx.scene        Scene)
  '(javafx.stage        Stage))

(require 'LogicElements)
(alias 'lel 'LogicElements)
(require 'LogicElementsDraw)
(alias 'ld 'LogicElementsDraw)
(require 'clojure.pprint)

(def schem-1
  {:mode :cursor
   :cursor-pos [6 5]
   :lels  {'G_000 {:type :in  :p [1 1] :direction :right}
           'G_001 {:type :out :p [5 1] :direction :right}
           'G_002 {:type :and :p [9 1] :direction :right :width 4 :height 4}}
   :geoms {'G_100 {:type :wire :p [[1 5] [5 5]]}
           'G_101 {:type :wire :p [[1 6] [5 6]]}
           'G_102 {:type :wire :p [[1 7] [5 7]]}
           'G_103 {:type :wire :p [[1 8] [5 8]]}
           'G_104 {:type :rect :p [[15 1] [20  5]]}
           'G_105 {:type :rect :p [[15 6] [20 10]]}}
   :rect-p0 [10 9]
   :selected-lels  #{'G_002}
   :selected-geoms {'G_100 #{0 1}
                    'G_101 #{0}
                    'G_102 #{1}
                    'G_105 #{[1 1] [0 1]}
                    }})

(def schem-2 (lel/move-mode    schem-1))
(def schem-3 (lel/move-mode    schem-1 true))
(def schem-4 (lel/add-mode     schem-1 :and))
(def schem-5 (lel/catalog-mode schem-1))

(defn -start [_ stage]
  (let [pane1 (Pane.) pane2 (Pane.) pane3 (Pane.) pane4 (Pane.)
        pane5 (Pane.)
        gridpane (GridPane.)]
    (.add gridpane pane1 0 0)
    (.add gridpane pane2 1 0)
    (.add gridpane pane3 2 0)
    (.add gridpane pane4 0 1)
    (.add gridpane pane5 1 1)
    (doto stage
      (.setWidth 800) (.setHeight 600)
      (.setScene (Scene. gridpane))
      (.setTitle "Unit Tests for LogicElementsDraw")
      (.show))
    (.setAll (.getChildren pane1) (ld/draw-mode schem-1))
    (.setAll (.getChildren pane2) (ld/draw-mode schem-2))
    (.setAll (.getChildren pane3) (ld/draw-mode schem-3))
    (.setAll (.getChildren pane4) (ld/draw-mode schem-4))
    (.setAll (.getChildren pane5) (ld/draw-mode schem-5))
    ))

(defn -main [& args]
  (Application/launch (Class/forName "tests.TestLogicElementsDraw")
                      (into-array String [])))
