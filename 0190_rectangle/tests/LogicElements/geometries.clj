(require 'LogicElements)
(alias 'dut 'LogicElements)

(def a-dff  {:type :dff :p [2 3] :width 4 :height 5})
(def a-wire {:type :wire :p [[3 5] [1 2]]})
(def test-patts
 [[(dut/x-min  a-dff ) 2]
  [(dut/x-max  a-dff ) 6]
  [(dut/y-min  a-dff ) 3]
  [(dut/y-max  a-dff ) 8]
  [(dut/width  a-dff ) 4]
  [(dut/height a-dff ) 5]
  [(dut/x-min  a-wire) 1]
  [(dut/x-max  a-wire) 3]
  [(dut/y-min  a-wire) 2]
  [(dut/y-max  a-wire) 5]
  [(dut/width  a-wire) 2]
  [(dut/height a-wire) 3]])

(doseq [[result expected] test-patts]
  (if (= result expected)
    (print "[OK]")
    (do
      (print "[ER] expected: ")
      (print expected)))
  (print " result: ")
  (println result))
