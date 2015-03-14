(ns tests.TestLogicElements)

(require 'LogicElements)
(alias 'dut 'LogicElements)

(def lels '{g-000 {:type :dff :x 10 :y 10 :width 4 :height 5}
            g-001 {:type :and :x 20 :y 10 :width 4 :height 4}})
(def wires '{g-100 {:x0 10 :y0 20 :x1 20 :y1 20}
             g-101 {:x0 10 :y0 20 :x1 10 :y1 30}})

(def test-patts [[(dut/rectangular-select lels wires
                   {:x 10 :y 10} {:x 14 :y 15})
                  {:lels #{'g-000} :geoms {}}]
                 [(dut/rectangular-select lels wires
                   {:x 10 :y 10} {:x 13 :y 15})
                  {:lels #{} :geoms {}}]
                 [(dut/rectangular-select lels wires
                   {:x 10 :y 10} {:x 14 :y 14})
                  {:lels #{} :geoms {}}]
                 [(dut/rectangular-select lels wires
                   {:x 20 :y 10} {:x 24 :y 14})
                  {:lels #{'g-001} :geoms {}}]
                 [(dut/rectangular-select lels wires
                   {:x 10 :y 10} {:x 24 :y 15})
                  {:lels #{'g-000 'g-001} :geoms {}}]
                 [(dut/rectangular-select lels wires
                   {:x 10 :y 20} {:x 20 :y 30})
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

