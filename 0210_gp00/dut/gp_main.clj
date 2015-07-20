(ns gp-main)

(use '[roulette    :as rl])
(use '[handle-gene :as hg])
(use '[run-gene    :as rg])
(use '[random-tree :as rt])

(def pool-size 2000)
(def gene-size-limit 100)
(def mutation-prob 0.01)

(defn init-pool []
  (map (fn [_] (rt:gen-tree 6))
       (range pool-size)))

;(defn score-gene [])

