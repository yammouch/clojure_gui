(ns tests.TestLogicElements)

(import '(javafx.scene.input KeyCode KeyEvent))

(require 'LogicElements)
(alias 'dut 'LogicElements)

;--------------------------------------------------
(println "tests rectangular-select")

(def lels '{g-000 {:type :dff :p [10 10] :width 4 :height 5}
            g-001 {:type :and :p [20 10] :width 4 :height 4}})
(def wires '{g-100 {:p [[10 20] [20 20]]}
             g-101 {:p [[10 20] [10 30]]}})

(def test-patts
  [[(dut/rectangular-select lels wires [10 10] [14 15])
    {:lels #{'g-000} :geoms {}}]
   [(dut/rectangular-select lels wires [10 10] [13 15])
    {:lels #{} :geoms {}}]
   [(dut/rectangular-select lels wires [10 10] [14 14])
    {:lels #{} :geoms {}}]
   [(dut/rectangular-select lels wires [20 10] [24 14])
    {:lels #{'g-001} :geoms {}}]
   [(dut/rectangular-select lels wires [10 10] [24 15])
    {:lels #{'g-000 'g-001} :geoms {}}]
   [(dut/rectangular-select lels wires [10 20] [20 30])
    {:lels #{} :geoms '{g-100 #{0 1} g-101 #{0 1}}}
    ]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK]")
    (do
      (print "[ER] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

;--------------------------------------------------
(println "tests move-cursor")

(def schem {:cursor-pos [10 20]})

(def test-patts
  [[(dut/move-cursor schem [ 2  0]) {:cursor-pos [12 20]}]
   [(dut/move-cursor schem [-2  0]) {:cursor-pos [ 8 20]}]
   [(dut/move-cursor schem [ 0  3]) {:cursor-pos [10 23]}]
   [(dut/move-cursor schem [ 0 -3]) {:cursor-pos [10 17]}]
   ])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK]")
    (do
      (print "[ER] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

;--------------------------------------------------
(println "tests move-selected")

(def schem
  {:moving-lels  {'G0 {:p [ 0  1]}
                  'G1 {:p [10 11]}}
   :moving-geoms {'G2 {:type :wire :p [[20 21] [22 23]]}
                  'G3 {:type :rect :p [[30 31] [32 33]]}}
   :geoms        {'G4 {:type :wire :p [[40 41] [42 43]]}
                  'G5 {:type :wire :p [[50 51] [52 53]]}
                  'G6 {:type :rect :p [[60 61] [62 63]]}
                  'G7 {:type :rect :p [[70 71] [72 73]]}
                  'G8 {:type :rect :p [[80 81] [82 83]]}
                  'G9 {:type :rect :p [[90 91] [92 93]]}}
   :moving-vertices {'G4 #{0}
                     'G5 #{1}
                     'G6 #{[0 0]}
                     'G7 #{[0 1]}
                     'G8 #{[1 0]}
                     'G9 #{[1 1]}
                     }})

(def test-patts
  [[(dut/move-selected schem [2 0])
    (-> schem
        (update-in [:moving-lels  'G0 :p  ] #(vec (map + [2 0] %)))
        (update-in [:moving-lels  'G1 :p  ] #(vec (map + [2 0] %)))
        (update-in [:moving-geoms 'G2 :p 0] #(vec (map + [2 0] %)))
        (update-in [:moving-geoms 'G2 :p 1] #(vec (map + [2 0] %)))
        (update-in [:moving-geoms 'G3 :p 0] #(vec (map + [2 0] %)))
        (update-in [:moving-geoms 'G3 :p 1] #(vec (map + [2 0] %)))
        (update-in [:geoms        'G4 :p 0] #(vec (map + [2 0] %)))
        (update-in [:geoms        'G5 :p 1] #(vec (map + [2 0] %)))
        (update-in [:geoms        'G6 :p 0] #(vec (map + [2 0] %)))
        (update-in [:geoms        'G8 :p 1] #(vec (map + [2 0] %))))]
   [(dut/move-selected schem [0 -3])
    (-> schem
        (update-in [:moving-lels  'G0 :p  ] #(vec (map + % [0 -3])))
        (update-in [:moving-lels  'G1 :p  ] #(vec (map + % [0 -3])))
        (update-in [:moving-geoms 'G2 :p 0] #(vec (map + % [0 -3])))
        (update-in [:moving-geoms 'G2 :p 1] #(vec (map + % [0 -3])))
        (update-in [:moving-geoms 'G3 :p 0] #(vec (map + % [0 -3])))
        (update-in [:moving-geoms 'G3 :p 1] #(vec (map + % [0 -3])))
        (update-in [:geoms        'G4 :p 0] #(vec (map + % [0 -3])))
        (update-in [:geoms        'G5 :p 1] #(vec (map + % [0 -3])))
        (update-in [:geoms        'G7 :p 0] #(vec (map + % [0 -3])))
        (update-in [:geoms        'G9 :p 1] #(vec (map + % [0 -3])))
        )]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK]")
    (do
      (print "[ER] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

;--------------------------------------------------
(println "tests move-mode")

(def schem
  {:mode  :cursor
   :lels  {'G0 :g0 'G1 :g1}
   :geoms {'G2 {:type :wire :p :g2}
           'G3 {:type :wire :p :g3}
           'G4 {:type :wire :p :g4}
           'G5 {:type :wire :p :g5}
           'G6 {:type :rect :p :g6}
           'G7 {:type :rect :p :g7}
           'G8 {:type :rect :p :g8}}
   :selected-lels #{'G0}
   :selected-geoms {'G2 #{0 1} 'G3 #{0} 'G4 #{1}
                    'G6 #{[0 0] [0 1] [1 0] [1 1]} 'G7 #{[0 0]}}
   })
(def schem-move-mode
  {:mode :move
   :lels {'G1 :g1}
   :geoms {'G3 {:type :wire :p :g3}
           'G4 {:type :wire :p :g4}
           'G5 {:type :wire :p :g5}
           'G7 {:type :rect :p :g7}
           'G8 {:type :rect :p :g8}}
   :moving-lels {'G0 :g0}
   :moving-geoms {'G2 {:type :wire :p :g2}
                  'G6 {:type :rect :p :g6}}
   :moving-vertices {'G3 #{0} 'G4 #{1} 'G7 #{[0 0]}}
   :revert-schem schem
   })
(def test-patts
  [[(dut/move-mode schem) schem-move-mode]
   [(dut/key-command-cursor-mode schem
     (KeyEvent. KeyEvent/KEY_PRESSED "m" "m" KeyCode/M
      false false false false))
    schem-move-mode]])

(def schem (reduce #(dissoc %1 %2) schem [:selected-lels :selected-geoms]))
(def test-patts
  (into test-patts
   [[(dut/move-mode schem) schem]
    [(dut/key-command-cursor-mode schem
      (KeyEvent. KeyEvent/KEY_PRESSED "m" "m" KeyCode/M
       false false false false))
     schem]]))

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK] ")
    (do
      (print "[ER]\nexpected: ")
      (println (into (sorted-map) expected))))
  (print "result  : ")
  (println (into (sorted-map) result)))

(def schem
  {:mode  :cursor
   :lels  {'G0 :g0 'G1 :g1}
   :geoms {'G2 {:type :wire :p :g2}
           'G3 {:type :wire :p :g3}
           'G4 {:type :wire :p :g4}
           'G5 {:type :wire :p :g5}
           'G6 {:type :rect :p :g6}
           'G7 {:type :rect :p :g7}
           'G8 {:type :rect :p :g8}}
   :selected-lels #{'G0}
   :selected-geoms {'G2 #{0 1} 'G3 #{0} 'G4 #{1}
                    'G6 #{[0 0] [0 1] [1 0] [1 1]} 'G7 #{[0 0]}
                    }})
(def schem-copy-mode
  {:mode :copy
   :lels {'G0 :g0 'G1 :g1}
   :geoms {'G2 {:type :wire :p :g2}
           'G3 {:type :wire :p :g3}
           'G4 {:type :wire :p :g4}
           'G5 {:type :wire :p :g5}
           'G6 {:type :rect :p :g6}
           'G7 {:type :rect :p :g7}
           'G8 {:type :rect :p :g8}}
   :moving-lels {'G0 :g0}
   :moving-geoms {'G2 {:type :wire :p :g2}
                  'G6 {:type :rect :p :g6}}
   :moving-vertices {}
   :revert-schem {}
   })
(def test-patts
  [[(dut/move-mode schem true) schem-copy-mode]
   [(dut/key-command-cursor-mode schem
     (KeyEvent. KeyEvent/KEY_PRESSED "c" "c" KeyCode/C
      false false false false))
    schem-copy-mode]])

(def schem (reduce #(dissoc %1 %2) schem [:selected-lels :selected-geoms]))
(def test-patts
  (into test-patts
   [[(dut/move-mode schem true) schem]
    [(dut/key-command-cursor-mode schem
      (KeyEvent. KeyEvent/KEY_PRESSED "c" "c" KeyCode/C
       false false false false))
     schem]]))

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK] ")
    (do
      (print "[ER]\nexpected: ")
      (println (into (sorted-map) expected))))
  (print "result  : ")
  (println (into (sorted-map) result)))

;--------------------------------------------------
(println "tests cursor mode, rectangular select ")

(def test-patts
  [[(dut/key-command-cursor-mode
     {:mode :cursor :cursor-pos :foo}
     (KeyEvent. KeyEvent/KEY_PRESSED "r" "r" KeyCode/R
      false false false false))
    {:mode :cursor :cursor-pos :foo :rect-p0 :foo}]
   [(dut/key-command-cursor-mode
     {:mode :cursor :cursor-pos :foo :rect-p0 :bar}
     (KeyEvent. KeyEvent/KEY_PRESSED "r" "r" KeyCode/R
      false false false false))
    {:mode :cursor :cursor-pos :foo}]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK] ")
    (do
      (print "[ER]\nexpected: ")
      (println (into (sorted-map) expected))))
  (print "result  : ")
  (println (into (sorted-map) result)))

;--------------------------------------------------
(println "tests add mode, pressing enter key")

(def test-patts
  [[(dut/key-command-add-mode
     {:mode :add :cursor-pos [1 2] :p [] :lel {:type :dff}
      :lels {} :geoms {}}
     (KeyEvent. KeyEvent/KEY_PRESSED "\n" "\n" KeyCode/ENTER
      false false false false))
    { :mode :add :cursor-pos [1 2] :p [] :lel {:type :dff}
      :lels {'g-0 {:type :dff :p [1 2]}} :geoms {}}]
   [(dut/key-command-add-mode
     {:mode :add :cursor-pos [1 2] :p [] :lel {:type :wire}
      :lels {} :geoms {}}
     (KeyEvent. KeyEvent/KEY_PRESSED "\n" "\n" KeyCode/ENTER
      false false false false))
    { :mode :add :cursor-pos [1 2] :p [[1 2]] :lel {:type :wire}
      :lels {} :geoms {}}]
   [(dut/key-command-add-mode
     {:mode :add :cursor-pos [3 4] :p [[1 2]] :lel {:type :wire}
      :lels {} :geoms {}}
     (KeyEvent. KeyEvent/KEY_PRESSED "\n" "\n" KeyCode/ENTER
      false false false false))
    { :mode :add :cursor-pos [3 4] :p [] :lel {:type :wire}
      :lels {} :geoms {'g-0 {:type :wire :p [[1 2] [3 4]]}}
      }]])

(defn gensym-map-to-set [schem]
  (-> schem
      (update-in [:lels ] #(into #{} (vals %)))
      (update-in [:geoms] #(into #{} (vals %)))
      (dissoc :undos)
      ))

(doseq [[result expected] test-patts]
  (if (= (gensym-map-to-set result)
         (gensym-map-to-set expected))
    (print "[OK] ")
    (do
      (print "[ER]\nexpected: ")
      (println (into (sorted-map) expected))))
  (print "result  : ")
  (println (into (sorted-map) result)))

