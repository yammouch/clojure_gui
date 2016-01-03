(ns xor)

(require 'mlp)
(require 'clojure.pprint)

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))
    ))

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
             :bv [0.0]}]
       rnd (map #(mod (mod % 131) (count training-data)) (lcg 1))]
  (when (= (mod i 10000) 0)
    (let [err (mlp/calc-err wbs training-data)]
      (printf "iter: %7d  avg err: %.3f  max err: %.3f\n"
              i (:avg err) (:max err)))
    (flush))
  (if (<= 4000000 i)
    (prn wbs)
    (recur (inc i)
           (mlp/learn1 wbs
                       ;(map (partial get training-data) (take 2 rnd))
                       [(nth training-data (mod i (count training-data)))]
                       0.025)
           (drop 2 rnd))))
