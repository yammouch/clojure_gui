(ns test.random-tree)

(require 'random-tree)
(alias 'rtr 'random-tree)
(require 'utils)
(alias 'ut 'utils)

(set! *print-length* 10)

(def test-cases [[8 3] [15 3] [1 3] [3 1] [3 15] [3 0]])

(doseq [x test-cases]
  (let [[result _] (rtr/distribute (x 0) (x 1) (ut/lcg 0))]
    (if (and (= (count result) (x 1))
             (or (= (reduce + result) (x 0))
                 (= (x 1) 0)))
      (print "[OK]")
      (print "[ER]"))
    (print " test case ")
    (println x)))

(println (rtr/gen-tree 4 (ut/lcg 0)))
