(ns comparator)

(require 'mlp)
(require 'clojure.pprint)

(def training-data
  [{:niv [0.0] :eov [0.0]}
   {:niv [1.0] :eov [0.0]}
   {:niv [2.0] :eov [1.0]}
   {:niv [3.0] :eov [1.0]}])

(loop [i 0 wbs (mlp/init-wbs [1 1])]
  (if (<= 1000 i)
    :done
    (let [wbs-next (mlp/learn1 wbs training-data 0.025)]
      (when (= (mod i 50) 0)
        (println "iter: " i)
        (clojure.pprint/pprint wbs))
      (recur (inc i) wbs-next))))
