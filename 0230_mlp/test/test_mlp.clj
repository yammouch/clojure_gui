(ns test-mlp)

(require 'mlp)

(def test-patterns
  [ [ [[2 0]
       [0 2]]
      [3 4] [6 8] ]
    [ [[1 2 3]
       [4 5 6]]
      [1 1 1] [6 15]
      ]])

(doseq [[m v exp] test-patterns]
  (let [result (mlp/m*v m v)]
    (if (= result exp)
      (print "[OK]")
      (print "[ER]" result))
    (println " test case " m v)
    ))
