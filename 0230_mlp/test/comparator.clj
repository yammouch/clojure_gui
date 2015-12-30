(ns comparator)

(require 'mlp)

(def training-data
  [{:niv [0.0] :eov [0.0]}
   {:niv [1.0] :eov [0.0]}
   {:niv [2.0] :eov [1.0]}
   {:niv [3.0] :eov [1.0]}])

(loop [i 0 wbs (mlp/init-wbs [1 1])]
  (when (= (mod i 1000) 0)
    (let [err (mlp/calc-err wbs training-data)]
      (printf "iter: %5d  avg err: %.3f  max err: %.3f\n"
              i (:avg err) (:max err))))
  (if (<= 10000 i)
    (prn wbs)
    (recur (inc i) (mlp/learn1 wbs training-data 0.025))))
