(ns test.roulette)

(require 'roulette)
(alias 'rlt 'roulette)

(def test-cases
 [{:in [[1 :a] [3 :b] [6 :c]] :exp [10 [[1 :a] [4 :b] [10 :c]]]}
  {:in [[1 :a] [1 :b] [1 :c]] :exp [ 3 [[1 :a] [2 :b] [ 3 :c]]]}
  {:in [[0 :a] [0 :b] [1 :c]] :exp [ 1 [[0 :a] [0 :b] [ 1 :c]]]}])
  
(doseq [x test-cases]
  (let [result (rlt/sumup (:in x))]
    (if (= (:exp x) result)
      (print "[OK]")
      (do (print "[ER]")
          (print result)))
    (print " test case ")
    (println (:in x))))

(let [f (rlt/make-roulette-wheel-selector (get-in test-cases 0 :in))]
  (println (->> (map (fn [_] (f)) (repeat 100 nil))
                frequencies sort)))
