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
