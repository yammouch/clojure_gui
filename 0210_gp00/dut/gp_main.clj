(ns gp-main)

(gen-class
  :name "gp_main"
  :main true)

(require 'utils)       (alias 'ut 'utils)
(require 'roulette)    (alias 'rl 'roulette)
(require 'handle-gene) (alias 'hg 'handle-gene)
(require 'run-gene)    (alias 'rg 'run-gene)
(require 'random-tree) (alias 'rt 'random-tree)

(def pool-size 500)
(def gene-size-limit 500)
(def mutation-prob 0.01)
(def max-iteration 20)

(defn init-pool [rnd-seq]
  (loop [i 0, rs rnd-seq, acc []]
    (if (<= pool-size i)
      [acc rs]
      (let [[tree rs-next] (rt/gen-tree 6 rs)]
        (recur (inc i) rs-next (conj acc tree))
        ))))

(defn score-schem [{[[x1 y1] [x2 y2]] :nodes}]
  (int (Math/floor (+ (/ 495.0 (+ (* 8 (Math/abs (- y2 y1)))
                                  (Math/abs (- x2 x1 8)) 5))
                      1))))

(def schem-1
  {:nodes [[10 10] [-10 -10]]
   :edges [[0 1]]
   })

(defn manip-schem [gene]
  (loop [schem schem-1 i 0]
    (if (<= max-iteration i)
      schem
      (let [[ret-val ret-schem] (rg/eval-gene gene schem)]
        (if (= ret-val :finish)
          ret-schem
          (recur ret-schem (inc i))
          )))))

(defn score-genes [pool]
  (map (comp score-schem manip-schem) pool))

(defn mutate [gene rnd]
  (let [nn (hg/count-node gene)
        mutant-src (rt/gen-tree 6)]
    (hg/replace-node gene (mod rnd nn) mutant-src)))

(defn next-pool [scored-pool rnd-seq]
  (let [roulette (rl/make-roulette-wheel-selector scored-pool)]
    (loop [retval [], rs rnd-seq]
      (if (<= pool-size (count retval))
        [retval rs]
        (let [[r0 r1 r2 r3 r4 r5] (take 6 rnd-seq)
              mother (roulette r0) mother-nn (hg/count-node mother)
              father (roulette r1) father-nn (hg/count-node father)
              [child _] (hg/crossover mother father
                                      (mod r2 mother-nn)
                                      (mod r3 father-nn))
              child (if (< (/ (mod r4 65536) 65536.0) mutation-prob)
                      (mutate child r5) child)]
          (recur (if (<= (hg/count-node child) gene-size-limit)
                   (conj retval child)
                   retval)
                 (drop 6 rnd-seq)
                 ))))))

(defn -main [& args]
  (loop [i 0 [pool rnd-seq] (init-pool (ut/lcg 0))]
    (if (<= 10000 i)
      pool
      (let [scores (score-genes pool)]
        (printf "generation: %6d , max score: %3d , average score: %5.1f\n"
                i (apply max scores)
                (/ (float (apply + scores)) (count scores)))
        (flush)
        ;(println i)
        (recur (inc i)
               (next-pool (map vector scores pool) rnd-seq)
               )))))
