(ns tests.TestLogicElementsDraw)

(gen-class
  :name "tests.TestLogicElementsDraw"
  :main true
  :extends javafx.application.Application)

(import
  '(javafx.application  Application)
  '(javafx.scene.layout Pane)
  '(javafx.scene        Scene)
  '(javafx.stage        Stage))

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
           'G_103 {:type :wire :p [[1 8] [5 8]]}}
   :rect-p0 [10 9]
   :selected-lels  #{'G_002}
   :selected-geoms {'G_100 #{0 1}
                    'G_101 #{0}
                    'G_102 #{1}
                    }})

(defn -start [_ stage]
  (let [pane (Pane.)]
    (doto stage
      (.setWidth 640) (.setHeight 480)
      (.setScene (Scene. pane))
      (.setTitle "Unit Tests for LogicElementsDraw")
      (.show))
    (.setAll (.getChildren pane) (ld/draw-mode schem-1))
    ))

(defn -main [& args]
  (Application/launch (Class/forName "tests.TestLogicElementsDraw")
                      (into-array String [])))
