(ns test.handle-gene)

(require 'handle-gene)
(alias 'hg 'handle-gene)

(def tree-1 '(:+ 1 2))
(def tree-2 '(:finish))
(def tree-3 '(:- (:nth (:boundary) 1) (:nth (:boundary) 0)))

(doseq [[tr n-node] [[tree-1 3] [tree-2 1] [tree-3 7]]]
  (let [result (hg/count-node tr)]
    (if (= result n-node)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println tr)))

(doseq [[i exp]
        [[0 tree-3]
         [1 '(:nth (:boundary) 1)]
         [2 '(:boundary)]
         [3 1]
         [4 '(:nth (:boundary) 0)]
         [5 '(:boundary)]
         [6 0]]]
  (let [result (hg/pickup-node tree-3 i)]
    (if (= result exp)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println i)))

(doseq [[i exp]
        [[0 :foo]
         [1 '(:- :foo (:nth (:boundary) 0))]
         [2 '(:- (:nth :foo 1) (:nth (:boundary) 0))]
         [3 '(:- (:nth (:boundary) :foo) (:nth (:boundary) 0))]
         [4 '(:- (:nth (:boundary) 1) :foo)]
         [6 '(:- (:nth (:boundary) 1) (:nth (:boundary) :foo))]
         ]]
  (let [result (hg/replace-node tree-3 i :foo)]
    (if (= result exp)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println i)))

;(def tree-1 '(:+ 1 2))
;(def tree-2 '(:finish))
;(def tree-3 '(:- (:nth (:boundary) 1) (:nth (:boundary) 0)))
(doseq [[pos1 pos2 exp-t1 exp-t3]
        [[0 0 tree-3 tree-1]
         [0 1 '(:nth (:boundary) 1) '(:- (:+ 1 2) (:nth (:boundary) 0))]
         [2 4 '(:+ 1 (:nth (:boundary) 0)) '(:- (:nth (:boundary) 1) 2)]
         ]]
  (let [[result-t1 result-t3] (hg/crossover tree-1 tree-3 pos1 pos2)]
    (if (and (= result-t1 exp-t1) (= result-t3 exp-t3))
      (print "[OK]")
      (do (print "[ER]")
          (print result-t1 result-t3)))
    (print " test case ")
    (println [pos1 pos2])))
