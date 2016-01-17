(ns test-schemaip)

(require 'schemanip)
(require 'clojure.pprint)
(alias 'smp 'schemanip)

(println "Tests slide-upper.")

(def test-patterns
  [{:arg  {:field {:size [2 3]
                   :body [[[0 0] [0 0]]
                          [[0 1] [0 0]]
                          [[0 0] [0 0]]]}
           :cmd {:cmd :move-y :org [1 2] :dst 1}}
    :expc {:field {:size [2 3]
                   :body [[[0 1] [0 0]]
                          [[0 0] [0 0]]
                          [[0 0] [0 0]]]}
           :cmd {:cmd :move-y :org [1 1] :dst 0}}}
   {:arg  {:field {:size [2 3]
                   :body [[[0 0] [0 0]]
                          [[0 1] [0 0]]
                          [[0 0] [0 0]]]}
           :cmd {:cmd :move-y :org [1 2] :dst 0}}
    :expc nil
    }])

(doseq [{arg :arg expc :expc} test-patterns]
  (let [result (smp/slide-upper arg)]
    (if (= result expc)
      (print "[OK]")
      (print "[ER]" result))
    (println " test case " arg)))

(println "Tests slide-lower.")

(def test-patterns
  [{:arg  {:field {:size [2 3]
                   :body [[[0 0] [0 0]]
                          [[0 1] [0 0]]
                          [[0 0] [0 0]]]}
           :cmd {:cmd :move-y :org [1 1] :dst 0}}
    :expc {:field {:size [2 3]
                   :body [[[0 0] [0 0]]
                          [[0 0] [0 0]]
                          [[0 1] [0 0]]]}
           :cmd {:cmd :move-y :org [1 2] :dst 1}}}
   {:arg  {:field {:size [2 3]
                   :body [[[0 0] [0 0]]
                          [[0 0] [0 0]]
                          [[0 0] [1 0]]]}
           :cmd {:cmd :move-y :org [1 1] :dst 0}}
    :expc nil}])

(doseq [{arg :arg expc :expc} test-patterns]
  (let [result (smp/slide-lower arg)]
    (if (= result expc)
      (print "[OK]")
      (print "[ER]" result))
    (println " test case " arg)))
