(ns xor)

(require 'mlp)
(require 'clojure.pprint)

(let [training-data [[[0.0 0.0] [0.0]]
                     [[0.0 1.0] [1.0]]
                     [[1.0 0.0] [1.0]]
                     [[1.0 1.0] [0.0]]
                     ]]
  (loop [i 0
         weight-array [[[0.0 0.0]
                        [0.0 0.0]]
                       [[0.0 0.0]]]
         b-array [[0.0 0.0] [0.0]]]
    (if (<= 10 i)
      :done
      (let [outputs (map (fn [[in _]]
                           (mlp/calc-output weight-array b-array in))
                         training-data)
            updates (map (fn [[out-array deriv-array] [_ expc]]
                           (mlp/calc-coeff-deriv weight-array deriv-array
                                                 out-array expc -0.025))
                         outputs training-data)]
        (println "iter:" i)
        (clojure.pprint/pprint [weight-array b-array])
        (recur (inc i)
               (apply mlp/m+ weight-array (map first updates))
               (apply map + b-array (map second updates))
               )))))
