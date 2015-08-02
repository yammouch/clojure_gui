(ns random-tree)

(gen-class
  :name "random-tree")

(use '[run-gene :as rg])
(def non-terminals
  (->> rg/fn-table
       (sort)
       (map (fn [[k v]] [k (:arity v)]))
       (filter (fn [[k arity]] (< 0 arity)))
       ))

(defn gen-terminal [rnd]
  (let [rnd (mod rnd 21)]
    (case rnd
      0                '(:finish)
      (- rnd 10))))

(defn distribute [i n rnd-seq]
  "Distributes i into n-element array. i must be > 0, n must be >= 0."
  (cond (<= n 0) [[] rnd-seq]
        (<= i 0) [(repeat n 0) rnd-seq]
        :else
        (let [rands (sort (concat [0 i]
                                  (map (fn [x] (mod x i))
                                       (take (dec n) rnd-seq)
                                       )))]
          [(map (fn [[x y]] (- y x)) (partition 2 1 rands))
           (drop (dec n) rnd-seq)
           ])))

(defn gen-tree [n-nt rnd-seq]
  "n-nt is the number of non-terminal"
  (if (<= n-nt 0)
    [(gen-terminal (first rnd-seq)) (next rnd-seq)]
    (let [[nt-symbol n-children]
          (nth non-terminals (mod (first rnd-seq) (count non-terminals)))
          [children rnd-seq]
          (loop [[nc rs] (distribute (dec n-nt) n-children (next rnd-seq))
                 acc []]
            (if (empty? nc)
              [acc rs]
              (let [[child rs-next] (gen-tree (first nc) rs)]
                (recur [(next nc) rs-next] (conj acc child))
                )))]
      [(cons nt-symbol children) rnd-seq])))
