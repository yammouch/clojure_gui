(ns unit-test)

(require 'schem-rtl)
(alias 'dut 'schem-rtl)

;--------------------------------------------------
; test for rectangular-select
;--------------------------------------------------

(def lels '{g-000 {:type dff :x 10 :y 10}
            g-001 {:type and :x 20 :y 10}})
(def wires '{g-100 {:x0 10 :y0 20 :x1 20 :y1 20}
             g-101 {:x0 10 :y0 20 :x1 10 :y1 30}})

(def test-patts [[(dut/rectangular-select lels wires 10 10 14 15)
                  {:lels #{'g-000} :wires #{}}]
                 [(dut/rectangular-select lels wires 10 10 13 15)
                  {:lels #{} :wires #{}}]
                 [(dut/rectangular-select lels wires 10 10 14 14)
                  {:lels #{} :wires #{}}]
                 [(dut/rectangular-select lels wires 20 10 24 14)
                  {:lels #{'g-001} :wires #{}}]
                 [(dut/rectangular-select lels wires 10 10 24 15)
                  {:lels #{'g-000 'g-001} :wires #{}}]
                 [(dut/rectangular-select lels wires 10 20 20 30)
                  {:lels #{} :wires #{'g-100 'g-101}}
                  ]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[ok]")
    (do
      (print "[er] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

;--------------------------------------------------
; test for find-wires-by-pos
;--------------------------------------------------

(def wires '{g-100 {:x0 10 :y0 20 :x1 20 :y1 20}
             g-101 {:x0 10 :y0 20 :x1 10 :y1 30}})

(def test-patts [[(dut/find-wires-by-pos wires {:x 10 :y 20})
                  {'g-100 'p0 'g-101 'p0}]
                 [(dut/find-wires-by-pos wires {:x 11 :y 20})
                  {'g-100 'p0}]
                 [(dut/find-wires-by-pos wires {:x 12 :y 20})
                  {'g-100 'p0p1}]
                 [(dut/find-wires-by-pos wires {:x 19 :y 20})
                  {'g-100 'p1}]
                 [(dut/find-wires-by-pos wires {:x 20 :y 20})
                  {'g-100 'p1}]
                 [(dut/find-wires-by-pos wires {:x 21 :y 20})
                  {}
                  ]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[ok]")
    (do
      (print "[er] expected: ")
      (print expected)))
  (print " result: ")
  (println result))

