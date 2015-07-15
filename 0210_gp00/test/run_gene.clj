(ns test.run-gene)

(require 'run-gene)
(alias 'rg 'run-gene)

(defn test-2-terms-op [f test-cases]
  (doseq [[[x y] exp] test-cases]
    (let [[result _] (f [x y] nil)]
      (if (= exp result)
        (print "[OK]")
        (print "[ER]" result))
      (print " test case ")
      (println [x y]))))

(println "test gene-+")
(test-2-terms-op rg/gene-+
                 [[[32000 767]  32767]
                  [[32000 768] -32768]
                  [[32000 769] -32767]])

(println "test gene--")
(test-2-terms-op rg/gene--
                 [[[-1 32766] -32767]
                  [[-1 32767] -32768]
                  [[-1 32768]  32767]])
  
(println "test gene-*")
(test-2-terms-op rg/gene-*
                 [[[256 256]    0]
                  [[255 256] -256]])
  
(println "test gene-div")
(test-2-terms-op rg/gene-div
                 [[[   256 256] 1]
                  [[131072   2] 0]])
  
(println "test gene-<")
(test-2-terms-op rg/gene-<
                 [[[32767 32768] 0]
                  [[32768 32767] 1]])
  
(println "test gene->")
(test-2-terms-op rg/gene->
                 [[[32767 32768] 1]
                  [[32768 32767] 0]])

(println "test gene-=")
(test-2-terms-op rg/gene-=
                 [[[32767 32768] 0]
                  [[65537     1] 1]])

(println "test gene-pos")

(def env-1
  {:nodes [[-5 -4]
           [ 3  2]
           [ 0  0]]
   :edges [[0 1]
           [1 2]]})

(def test-cases [[0 [-5 -4]]
                 [1 [ 3  2]]
                 [2 [ 0  0]]])

(doseq [[inst-id exp] test-cases]
  (let [[result _] (rg/gene-pos [inst-id] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println inst-id)))
