(ns tests.TestLogicElements)

(require 'LogicElements)
(alias 'dut 'LogicElements)

;--------------------------------------------------
(println "tests rectangular-select")

(def lels '{g-000 {:type :dff :x 10 :y 10 :width 4 :height 5}
            g-001 {:type :and :x 20 :y 10 :width 4 :height 4}})
(def wires '{g-100 {:x0 10 :y0 20 :x1 20 :y1 20}
             g-101 {:x0 10 :y0 20 :x1 10 :y1 30}})

(def test-patts
  [[(dut/rectangular-select lels wires {:x 10 :y 10} {:x 14 :y 15})
    {:lels #{'g-000} :geoms {}}]
   [(dut/rectangular-select lels wires {:x 10 :y 10} {:x 13 :y 15})
    {:lels #{} :geoms {}}]
   [(dut/rectangular-select lels wires {:x 10 :y 10} {:x 14 :y 14})
    {:lels #{} :geoms {}}]
   [(dut/rectangular-select lels wires {:x 20 :y 10} {:x 24 :y 14})
    {:lels #{'g-001} :geoms {}}]
   [(dut/rectangular-select lels wires {:x 10 :y 10} {:x 24 :y 15})
    {:lels #{'g-000 'g-001} :geoms {}}]
   [(dut/rectangular-select lels wires {:x 10 :y 20} {:x 20 :y 30})
    {:lels #{} :geoms '{g-100 #{:p0 :p1} g-101 #{:p0 :p1}}}
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

(def schem {:cursor-pos {:x 10 :y 20}})

(def test-patts
  [[(dut/move-cursor schem :x  2) {:cursor-pos {:x 12 :y 20}}]
   [(dut/move-cursor schem :x -2) {:cursor-pos {:x  8 :y 20}}]
   [(dut/move-cursor schem :y  3) {:cursor-pos {:x 10 :y 23}}]
   [(dut/move-cursor schem :y -3) {:cursor-pos {:x 10 :y 17}}]
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
  {:moving-lels  {'G0 {:x   0 :y   1}
                  'G1 {:x  10 :y  11}}
   :moving-geoms {'G2 {:x0 20 :y0 21 :x1 22 :y1 23}
                  'G3 {:x0 30 :y0 31 :x1 32 :y1 33}}
   :geoms        {'G4 {:x0 40 :y0 41 :x1 42 :y1 43}
                  'G5 {:x0 50 :y0 51 :x1 52 :y1 53}}
   :moving-vertices {'G4 #{':p0}
                     'G5 #{':p1}
                     }})

(def test-patts
  [[(dut/move-selected schem :x 2)
    (-> schem
        (update-in [:moving-lels  'G0 :x ] #(+ 2 %))
        (update-in [:moving-lels  'G1 :x ] #(+ 2 %))
        (update-in [:moving-geoms 'G2 :x0] #(+ 2 %))
        (update-in [:moving-geoms 'G2 :x1] #(+ 2 %))
        (update-in [:moving-geoms 'G3 :x0] #(+ 2 %))
        (update-in [:moving-geoms 'G3 :x1] #(+ 2 %))
        (update-in [:geoms        'G4 :x0] #(+ 2 %))
        (update-in [:geoms        'G5 :x1] #(+ 2 %)))]
   [(dut/move-selected schem :y -3)
    (-> schem
        (update-in [:moving-lels  'G0 :y ] #(- % 3))
        (update-in [:moving-lels  'G1 :y ] #(- % 3))
        (update-in [:moving-geoms 'G2 :y0] #(- % 3))
        (update-in [:moving-geoms 'G2 :y1] #(- % 3))
        (update-in [:moving-geoms 'G3 :y0] #(- % 3))
        (update-in [:moving-geoms 'G3 :y1] #(- % 3))
        (update-in [:geoms        'G4 :y0] #(- % 3))
        (update-in [:geoms        'G5 :y1] #(- % 3))
        )]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK]")
    (do
      (print "[ER] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

