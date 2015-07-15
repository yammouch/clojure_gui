(ns test.run-gene)

(require 'run-gene)
(alias 'rg 'run-gene)

(println "test gene-+")

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

(println "test gene--")

(def test-cases
 [[[-1 32766] -32767]
  [[-1 32767] -32768]
  [[-1 32768]  32767]])
  
(doseq [[[x y] exp] test-cases]
  (let [[result _] (rg/gene-- [x y] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println [x y])))

(println "test gene-*")

(def test-cases
 [[[256 256]    0]
  [[255 256] -256]])
  
(doseq [[[x y] exp] test-cases]
  (let [[result _] (rg/gene-* [x y] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println [x y])))

(println "test gene-div")

(def test-cases
 [[[   256 256] 1]
  [[131072   2] 0]])
  
(doseq [[[x y] exp] test-cases]
  (let [[result _] (rg/gene-div [x y] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println [x y])))
