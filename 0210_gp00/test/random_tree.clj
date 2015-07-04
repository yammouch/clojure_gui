(ns test.random-tree)

(require 'random-tree)
(alias 'rtr 'random-tree)

(def test-cases [[8 3] [15 3] [1 3] [3 1] [3 15]])

(doseq [x test-cases]
  (let [result (apply rtr/distribute x)]
    (if (and (= (count result) (x 1))
             (= (reduce + result) (x 0)))
      (print "[OK]")
      (print "[ER]"))
    (print " test case ")
    (println x)))
       
