(ns xor)

(require 'mlp)
(require 'clojure.pprint)


(def training-data
  [{:niv [0.0 0.0] :eov [0.0]}
   {:niv [0.0 1.0] :eov [1.0]}
   {:niv [1.0 0.0] :eov [1.0]}
   {:niv [1.0 1.0] :eov [0.0]}])

(loop [i 0
       wbs [{:wm [[0.0 0.0]
                  [0.0 0.1]
                  [0.0 0.0]]
             :bv [0.1 0.0 -0.1]}
            {:wm [[0.1  0.0 0.0]
                  [0.0 -0.1 0.0]
                  [0.0  0.0 0.0]]
             :bv [0.0 0.0 0.0]}
            {:wm [[0.0 0.0 0.0]]
             :bv [0.0]}]]
  (when (= (mod i 100000) 0)
    (let [err (mlp/calc-err wbs training-data)]
      (printf "iter: %7d  avg err: %.3f  max err: %.3f\n"
              i (:avg err) (:max err))))
  (if (<= 4000000 i)
    (prn wbs)
    (recur (inc i) (mlp/learn1 wbs [(nth training-data (mod i 4))] 0.025))))
