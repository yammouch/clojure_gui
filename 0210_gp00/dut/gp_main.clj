(ns gp-main)

(use '[roulette    :as rl])
(use '[handle-gene :as hg])
(use '[run-gene    :as rg])
(use '[random-tree :as rt])

(def rand-obj (java.util.Random. 1))

(def pool-size 2000)
(def gene-size-limit 100)
(def mutation-prob 0.01)

(defn init-pool []
  (map (fn [_] (rt:gen-tree 6))
       (range pool-size)))

;(defn score-genes [pool])

(defn mutate [gene]
  (let [nn (hg/count-node gene)
        mutant-src (rt/gen-tree 6)]
    (hg/replace-node gene (.nextInt rand-obj nn) mutant-src)))

(defn next-pool [scored-pool]
  (loop [roulette (rl/make-roulette-wheel-selector scored-pool
         num 0 retval []]
    (let [mother (roulette) mother-nn (count-mode mother)
          father (roulette) father-nn (count-node father)
          child (hg/crossover mother father
                              (.nextInt rand-obj mother-nn)
                              (.nextInt rand-obj father-nn))
          child ()]
