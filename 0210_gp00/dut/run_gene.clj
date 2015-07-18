; (if pred t f)
; (< x y)
; (> x y)
; (= x y)
; (+ x y)
; (- x y)
; (* x y)
; (/ x y)
; (pos instance-id)
; (nth seq n)
; (setx instance-id x)
; (sety instance-id y)
; (adjacents instance-id)
; (prog2 s0 s1)
; (prog3 s0 s1 s2)
; (boundary)

(ns run-gene)

(defn into-16bits [x]
  (let [m (mod x 65536)]
    (if (<= 32768 m)
      (- m 65536)
      m)))

(defn normalize-inst-id [inst-id env]
  (if (coll? inst-id)
    0 (mod inst-id (count (:nodes env)))
    ))

(defn bool-to-int [x] (if x 1 0))

(defn gene-< [[x y] env]
  [(bool-to-int (apply < (map into-16bits [x y]))) env])
(defn gene-> [[x y] env]
  [(bool-to-int (apply > (map into-16bits [x y]))) env])
(defn gene-= [[x y] env]
  [(bool-to-int (apply = (map into-16bits [x y]))) env])
(defn gene-+ [[x y] env] [(into-16bits (+ x y)) env])
(defn gene-- [[x y] env] [(into-16bits (- x y)) env])
(defn gene-* [[x y] env] [(into-16bits (* x y)) env])
(defn gene-div [[x y] env]
  [(into-16bits (int (Math/floor (/ x y)))) env])

(defn gene-pos [[inst-id] env]
  [(get-in env [:nodes (if (coll? inst-id) 0 inst-id)])
   env])
(defn gene-nth [[l n] env]
  [(if (coll? l)
     (let [l-length (count l)]
       (if (<= l-length 0) 0 (nth l (mod n l-length))))
     0)
   env])

(defn gene-setpos [axis inst-id pos env]
  (assoc-in env [:nodes (normalize-inst-id inst-id env) axis] pos))
(defn gene-setx [[inst-id x] env] [0 (gene-setpos 0 inst-id x env)])
(defn gene-sety [[inst-id y] env] [0 (gene-setpos 1 inst-id y env)])

(defn gene-adjacents [[inst-id] env]
  (let [ii (normalize-inst-id inst-id env)]
    [(->> (:edges env)
          (filter (fn [iis-edge] (some (partial = ii) iis-edge)))
          (apply concat)
          distinct
          (remove (partial = ii)))
     env]))

(defn gene-prog2 [[x0 x1] env] [x1 env])
(defn gene-prog3 [[x0 x1 x2] env] [x2 env])

(defn gene-boundary [_ env]
  (let [[xs ys] (apply map vector (:nodes env))]
    [[(apply min xs) (apply min ys) (apply max xs) (apply max ys)]
     env]))
 
(def fn-table {})

(def eval-gene)

(defn eval-if [gene env]
  (let [[pred-result env-new] (eval-gene (nth gene 1) env)]
    (if (= 0 pred-result)
      (eval-gene (nth gene 2) env-new)
      (eval-gene (nth gene 3) env-new))))

(defn eval-gene [gene env]
  (if (coll? gene)
    (case (first gene)
      :if (eval-if gene env)
      (apply (fn-table (first gene))
             (loop [args (next gene) evaled-args [] env env]
               (if (nil? args)
                 [evaled-args env]
                 (let [[evaled-arg env-new] (eval-gene (first args) env)]
                   (recur (next args)
                          (conj evaled-args evaled-arg)
                          env-new))))))
    gene)) 
