(ns gp-main)

(require 'roulette)    (alias 'rl 'roulette)
(require 'handle-gene) (alias 'hg 'handle-gene)
(require 'run-gene)    (alias 'rg 'run-gene)
(require 'random-tree) (alias 'rt 'random-tree)
;(use '[roulette    :as rl])
;(use '[handle-gene :as hg])
;(use '[run-gene    :as rg])
;(use '[random-tree :as rt])

(def rand-obj (java.util.Random. 1))

(def pool-size 2000)
(def gene-size-limit 100)
(def mutation-prob 0.01)
(def max-iteration 20)

(defn init-pool []
  (map (fn [_] (rt/gen-tree 6))
       (range pool-size)))

(defn score-schem [{[[x1 y1] [x2 y2]] :nodes}]
  (int (Math/floor (/ 500.0 (+ (* 8 (Math/abs (- y2 y1)))
                               (Math/abs (- x2 x1 8)) 5)))))

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

(defn mutate [gene]
  (let [nn (hg/count-node gene)
        mutant-src (rt/gen-tree 6)]
    (hg/replace-node gene (.nextInt rand-obj nn) mutant-src)))

(defn next-pool [scored-pool]
  (let [roulette (rl/make-roulette-wheel-selector scored-pool)]
    (loop [retval []]
      (if (<= pool-size (count retval))
        retval
        (let [mother (roulette) mother-nn (hg/count-node mother)
              father (roulette) father-nn (hg/count-node father)
              [child _] (hg/crossover mother father
                                      (.nextInt rand-obj mother-nn)
                                      (.nextInt rand-obj father-nn))
              child (if (< (/ (.nextInt rand-obj 65536) 65536.0)
                           mutation-prob)
                      (mutate child) child)]
          (recur (if (<= (hg/count-node child) gene-size-limit)
                   (conj retval child)
                   retval)))))))

(defn -main [& args]
  (loop [i 0 pool (init-pool)]
    (if (<= 100 i)
      pool
      (let [scores (score-genes pool)]
        (println "generation: " i ", max score: " (apply max scores))
        (recur (inc i)
               (next-pool (map vector scores pool))
               )))))
