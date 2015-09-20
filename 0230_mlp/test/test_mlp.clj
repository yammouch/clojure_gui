(ns test-mlp)

(require 'mlp)
(require 'clojure.pprint)

(println "Tests m*v.")

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


(println "Tests cv*rv.")

(def test-patterns
  [ [ [1 2 3] [4 5 6]
      [ [ 4  5  6]
        [ 8 10 12]
        [12 15 18]
        ]]
    [ [1 2] [3 4 5]
      [ [3 4  5]
        [6 8 10]
        ]]])

(doseq [[cv rv exp] test-patterns]
  (let [result (mlp/cv*rv cv rv)]
    (if (= result exp)
      (print "[OK]")
      (print "[ER]" result))
    (println " test case " cv rv)
    ))


(println "Tests calc-output.")

(let [weight-array [[[0.0 0.0]
                     [0.0 0.0]]
                    [[0.0 0.0]]]
      b-array [[0.0 0.0]
               [0.0]]
      invec [1.0 0.0]
      [out-array deriv-array] (mlp/calc-output weight-array b-array invec)]
  (clojure.pprint/pprint [out-array deriv-array])
  (clojure.pprint/pprint
   (mlp/calc-coeff-deriv weight-array deriv-array out-array [1.0] 1.0)
   ))

