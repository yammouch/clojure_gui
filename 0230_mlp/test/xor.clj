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
       wbs [{:wm [[2.0 2.0]
                  [5.8 5.8]
                  [2.6 2.6]]
             :bv [-3.2 -2.4 -4.1]}
            {:wm [[2.1 -3.8 2.6]
                  [3.3 -5.4 4.2]
                  [1.8 -3.8 2.4]]
             :bv [0.8 1.4 0.9]}
            {:wm [[-4.5 -7.3 -4.3]]
             :bv [6.5]}]
       rnd (map #(mod % (count training-data)) (lcg 1))]
  (when (= (mod i 10000) 0)
    (let [err (mlp/calc-err wbs training-data)]
      (printf "iter: %7d  avg err: %.3f  max err: %.3f\n"
              i (:avg err) (:max err)))
      (prn wbs)
    (flush))
  (if (<= 10000000 i)
  ;(if (<= 4 i)
    (prn wbs)
    (recur (inc i)
           (mlp/learn1 wbs
                       ;(map (partial get training-data) (take 2 rnd))
                       [(nth training-data (mod i (count training-data)))]
                       0.025)
           (drop 2 rnd))))
