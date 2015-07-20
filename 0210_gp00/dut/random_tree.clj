(ns random-tree)

(use '[run-gene :as rg])
(def non-terminals
  (->> rg/fn-table
       (sort)
       (map (fn [[k v]] [k (:arity v)]))
       (filter (fn [[k arity]] (< 0 arity)))
       ))

(def rand-obj (java.util.Random. 1)) ; arg: seed

(defn gen-terminal []
  (let [x (.nextInt rand-obj 128)]
    (case x
      0                '(:finish)
      (int (Math/floor (+ 0.5 (* 8 (.nextGaussian rand-obj)))))
      )))

(defn distribute [i n]
  "Distributes i into n-element array. i must be > 0, n must be >= 0."
  (cond (<= n 0) []
        (<= i 0) (repeat n 0)
        :else
        (let [rands (sort (concat [0 i]
                                  (map (fn [_] (.nextInt rand-obj i))
                                       (range (dec n))
                                       )))]
          (map (fn [[x y]] (- y x)) (partition 2 1 rands))
          )))

(defn gen-tree [n-nt]
  "n-nt is the number of non-terminal"
  (if (<= n-nt 0)
    (gen-terminal)
    (let [[nt-symbol n-children]
          (nth non-terminals (.nextInt rand-obj (count non-terminals)))]
      (cons nt-symbol
            (map gen-tree
                 (distribute (dec n-nt) n-children)
                 )))))
