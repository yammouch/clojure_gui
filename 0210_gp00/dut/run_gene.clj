(ns run-gene)

(gen-class
  :name "run_gene")

; In the world for genes, numbers are restricted from -2^15 to 2^15-1
(defn to-num [x]
  (cond (number? x) (let [m (mod x 65536)]
                      (if (<= 32768 m) (- m 65536) m))
        (true? x) 1
        :else 0))

(defn normalize-inst-id [inst-id env]
  (if (coll? inst-id)
    0 (mod inst-id (count (:nodes env)))
    ))

(defn gene-op [[op-type x y] env]
  (let [fn-op (case (mod op-type 7)
                0 <, 1 >, 2 =, 3 +, 4 -, 5 *,
                6 (fn [x y]
                    (if (zero? y)
                      0
                      (int (Math/floor (/ x y)))
                      )))]
    [(to-num (apply fn-op (map to-num [x y]))) env]))

(defn gene-pos [[inst-id axes] env]
  [(get-in env [:nodes (normalize-inst-id inst-id env) (mod axes 2)])
   env])

(defn gene-mov [[inst-id axes amount] env]
  (let [refer [:nodes (normalize-inst-id inst-id env) (mod axes 2)]
        moved-pos (+ (get-in env refer) amount)]
    [0 (assoc-in env refer (cond (<= 32767 moved-pos)  32767
                                 (<= moved-pos -32768) -32768
                                 :else                 moved-pos))]))

(defn gene-adjacent [[inst-id i] env]
  (let [ii (normalize-inst-id inst-id env)
        adjacents (->> (:edges env)
                       (filter (fn [iis-edge]
                                 (some (partial = ii) iis-edge)))
                       (apply concat)
                       distinct
                       (remove (partial = ii)))]
    [(nth adjacents (mod i (count adjacents)))
     env]))

(defn gene-boundary [[axes dir] env]
  (let [axes (mod axes 2)]
    [(apply (case (mod dir 2) 0 min 1 max)
            (map #(% axes) (:nodes env)))
     env]))
 
(def fn-table
 {:op       {:fn gene-op       :arity 3} ; (:op op-type x y)
  :pos      {:fn gene-pos      :arity 2} ; (:pos inst-id axes)
  :mov      {:fn gene-mov      :arity 3} ; (:mov inst-id axes amount)
  :adjacent {:fn gene-adjacent :arity 2} ; (:adjacent inst-id i)
  :boundary {:fn gene-boundary :arity 2} ; (:boundary axes dir)
  :prog2    {:fn nil           :arity 2} ; (:prog2 s0 s1) - special form
  :prog3    {:fn nil           :arity 3} ; (:prog3 s0 s1 s2) - special form
  :if       {:fn nil           :arity 3} ; (:if pred t f) - special form
  :finish   {:fn nil           :arity 0} ; (:finish) - special form
  })

(def eval-gene)

(defn eval-prog [genes env]
  (let [[result result-env] (eval-gene (first genes) env)]
    (cond (= result :finish) [:finish result-env]
          (nil? (next genes)) [result result-env]
          :else (recur (next genes) result-env)
          )))

(defn eval-if [gene env]
  (let [[pred-result env-new] (eval-gene (nth gene 1) env)]
    (cond (= pred-result :finish) [:finish env-new]
          (= 0 pred-result) (eval-gene (nth gene 3) env-new) ; false clause
          :else (eval-gene (nth gene 2) env-new) ; true clause
          )))

(defn eval-gene [gene env]
  (if (coll? gene)
    (case (first gene)
      :if (eval-if gene env)
      :prog2 (eval-prog (next gene) env)
      :prog3 (eval-prog (next gene) env)
      :finish [:finish env]
      (let [[args env]
            (loop [args (next gene) evaled-args [] env env]
              (if (nil? args)
                [evaled-args env]
                (let [[evaled-arg env-new] (eval-gene (first args) env)]
                  (if (= evaled-arg :finish)
                    [:finish env-new]
                    (recur (next args)
                           (conj evaled-args evaled-arg)
                           env-new)))))]
        (if (= args :finish)
          [:finish env]
          (apply (get-in fn-table [(first gene) :fn])
                 [args env]))))
    [gene env]))
