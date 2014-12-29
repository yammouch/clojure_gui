(ns unit-test)

(require 'rectangular-select)
(alias 'dut 'rectangular-select)

(def lels '{g-000 {:type dff :x 10 :y 10}
            g-001 {:type and :x 20 :y 10}})
(def wires '{g-100 {:x0 10 :y0 20 :x1 20 :y1 20}
             g-101 {:x0 10 :y0 20 :x1 10 :y1 30}})

(let [test-patts [[(dut/rectangular-select 10 10 14 15)
                   {:lels #{'g-000} :wires #{}}]
                  [(dut/rectangular-select 10 10 13 15)
                   {:lels #{} :wires #{}}]
                  [(dut/rectangular-select 10 10 14 14)
                   {:lels #{} :wires #{}}]
                  [(dut/rectangular-select 20 10 24 14)
                   {:lels #{'g-001} :wires #{}}]
                  [(dut/rectangular-select 10 10 24 15)
                   {:lels #{'g-000 'g-001} :wires #{}}]
                  [(dut/rectangular-select 10 20 20 30)
                   {:lels #{} :wires #{'g-100 g-101}}]
  (doseq [[result expected] test-patts]
    (if (= res exp)
      (print "[OK]")
      (do
        (print "[ER] expected: ")
        (print expected)))
    (print " result: ")
    (println result)))

