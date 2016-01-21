(ns disentangler)

(require 'mlp)
(require 'schemanip)
(alias 'smp 'schemanip)

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))
    ))

(def training-data
  (->> {:field (read-string (slurp "td000.txt"))
        :cmd   {:cmd :move-y :org [9 6] :dst 9}}
       smp/expand
       (map smp/mlp-input)
       vec))

(loop [i 0
       wbs (mlp/init-wbs [(count (:niv (first training-data)))
                          100 100
                          (count (:eov (first training-data)))]
                         (map #(* 1e-3 (- (mod % 201) 100))
                                        (lcg 1)))
       rnd (map #(mod (mod % 63529) (count training-data)) (lcg 1))]
  (when true ;(= (mod i 100) 0)
    (let [err (mlp/calc-err wbs training-data)]
      (printf "iter: %7d  avg err: %.3f  max err: %.3f\n"
              i (:avg err) (:max err)))
    (flush))
  (when (= (mod i 100) 0)
    (spit (format "wbs/wbs_%05d.txt" i) (doall wbs)))
  (if (<= 10000 i)
    (prn wbs)
    (recur (inc i)
           (mlp/learn1 wbs
                       (map (partial get training-data) (take 16 rnd))
                       0.05)
           (drop 16 rnd))))
