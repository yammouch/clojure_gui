(ns test.run-gene)

(require 'run-gene)
(alias 'rg 'run-gene)

(def test-cases
 [[[32000 767]  32767]
  [[32000 768] -32768]
  [[32000 769] -32767]])
  
(doseq [[[x y] exp] test-cases]
  (let [[result _] (rg/gene-+ [x y] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println [x y])))
