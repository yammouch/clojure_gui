(ns handle-gene)

(gen-class
  :name "handle_gene")

(use '[run-gene :as rg])

(defn terminal? [x] (not (coll? x)))

(defn non-terminal? [x]
  (and (coll? x)
       (contains? rg/fn-table (first x))))

(defn grec-num
  ([tree term-fn non-term-fn]
   (grec-num tree term-fn non-term-fn (fn [_ l r _] (cons l r)) (fn [_] nil)))
  ([tree term-fn non-term-fn cons-fn]
   (grec-num tree term-fn non-term-fn cons-fn (fn [_] nil)))
  ([tree term-fn non-term-fn cons-fn null-fn]
   (letfn [(rec [tr i]
             (cond (nil? tr) [(null-fn i) i]
                   (terminal? tr) [(term-fn tr i) (inc i)]
                   (non-terminal? tr)
                   (let [[children next-i] (rec (next tr) (inc i))]
                     [(non-term-fn tr children i) next-i])
                   :else
                   (let [[child1   next-i1] (rec (first tr) i      )
                         [children next-i2] (rec (next  tr) next-i1)]
                     [(cons-fn tr child1 children i) next-i2]
                     )))]
     (rec tree 0))))

(defn count-node [tree]
  (second (grec-num tree (fn [& _] nil) (fn [& _] nil) (fn [& _] nil))))

(defn pickup-node [tree n]
  (first (grec-num tree (fn [tr i] (if (= i n) tr))
                        (fn [tr children i] (if (= i n) tr children))
                        (fn [_ l r _] (if l l r)))))

(defn replace-node [tree n new-node]
  (first (grec-num tree
                   (fn [tr i] (if (= i n) new-node tr))
                   (fn [tr children i]
                     (if (= i n) new-node (cons (first tr) children))
                     ))))

(defn crossover [t1 t2 pos1 pos2]
  [(replace-node t1 pos1 (pickup-node t2 pos2))
   (replace-node t2 pos2 (pickup-node t1 pos1))])
