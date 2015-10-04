(ns comparator)

(require 'mlp)
(require 'clojure.pprint)


(def training-data [[[0.0] [0.0]]
                    [[1.0] [0.0]]
                    [[2.0] [1.0]]
                    [[3.0] [1.0]]
                    ])

(loop [i 0
       td (cycle training-data)
       weight-array [[[0.0]]]
       b-array [[0.0]] ]
  (if (<= 10000 i)
    :done
    (let [outputs (map (fn [[in _]]
                         (mlp/calc-output weight-array b-array in))
                       (take 1 training-data))
          updates (map (fn [[out-array deriv-array] [_ expc]]
                         (mlp/calc-coeff-deriv weight-array deriv-array
                                               out-array expc -0.025))
                       outputs (take 1 training-data))]
      (println "iter:" i)
      ;(clojure.pprint/pprint outputs)
      ;(clojure.pprint/pprint updates)
      (clojure.pprint/pprint [weight-array b-array])
      ;(clojure.pprint/pprint (first (first updates)))
      ;(clojure.pprint/pprint (mlp/r+ weight-array (first (first updates))))
      (recur (inc i)
             (drop 1 training-data)
             (apply mlp/r+ weight-array (map first updates))
             (apply mlp/r+ b-array (map second updates))
             ))))
