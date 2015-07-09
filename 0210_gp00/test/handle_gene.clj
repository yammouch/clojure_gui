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
