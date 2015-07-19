(ns test.handle-gene)

(require 'handle-gene)
(alias 'hg 'handle-gene)

(def tree-1 '(:op 4 1 2))
(def tree-2 10)
(def tree-3 '(:op 5 (:boundary 0 0) (:boundary 1 0)))

(doseq [[tr n-node] [[tree-1 4] [tree-2 1] [tree-3 8]]]
  (let [result (hg/count-node tr)]
    (if (= result n-node)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println tr)))

(doseq [[i exp]
        [[0 tree-3]
         [1 5]
         [2 '(:boundary 0 0)]
         [3 0]
         [4 0]
         [5 '(:boundary 1 0)]
         [6 1]
         [7 0]]]
  (let [result (hg/pickup-node tree-3 i)]
    (if (= result exp)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println i)))

(doseq [[i exp]
        [[0 :foo]
         [1 '(:op :foo (:boundary 0 0) (:boundary 1 0))]
         [2 '(:op 5 :foo (:boundary 1 0))]
         [3 '(:op 5 (:boundary :foo 0) (:boundary 1 0))]
         [4 '(:op 5 (:boundary 0 :foo) (:boundary 1 0))]
         [5 '(:op 5 (:boundary 0 0) :foo)]
         [6 '(:op 5 (:boundary 0 0) (:boundary :foo 0))]
         [7 '(:op 5 (:boundary 0 0) (:boundary 1 :foo))]
         ]]
  (let [result (hg/replace-node tree-3 i :foo)]
    (if (= result exp)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println i)))

;(def tree-1 '(:op 4 1 2))
;(def tree-3 '(:- (:boundary 0 0) (:boundary 1 0)))
(doseq [[pos1 pos2 exp-t1 exp-t3]
        [[0 0 tree-3 tree-1]
         [0 2 '(:boundary 0 0) '(:op 5 (:op 4 1 2) (:boundary 1 0))]
         [2 5 '(:op 4 (:boundary 1 0) 2) '(:op 5 (:boundary 0 0) 1)]
         ]]
  (let [[result-t1 result-t3] (hg/crossover tree-1 tree-3 pos1 pos2)]
    (if (and (= result-t1 exp-t1) (= result-t3 exp-t3))
      (print "[OK]")
      (do (print "[ER]")
          (print result-t1 result-t3)))
    (print " test case ")
    (println [pos1 pos2])))
