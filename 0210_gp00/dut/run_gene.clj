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

(defn gene-+ [[x y] env] [(into-16bits (+ x y)) env])
(defn gene-- [[x y] env] [(into-16bits (- x y)) env])
(defn gene-* [[x y] env] [(into-16bits (* x y)) env])
(defn gene-div [[x y] env]
  [(into-16bits (int (Math/floor (/ x y)))) env])
 
(def fn-table {})
(defn eval-if [gene env]) 

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
