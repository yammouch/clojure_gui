(ns mlp)

(defn r+2 [x y] ; recursive +, for matrix, matrix array, vector etc.
  (cond (nil? x) []
        (coll? x) (if (empty? x)
                    []
                    (cons (r+2 (first x) (first y))
                          (r+2 (next x) (next y))))
        :else (+ x y)))
(defn r+ [& xs] (reduce r+2 xs))

(defn m*v [m v] (map #(reduce + (map * % v)) m))
(defn cv*rv [c r] ; c: column vector, r: row vector
  (map (fn [ce] (map (fn [re] (* ce re))
                     r))
       c))
(defn transpose [m] (apply map vector m))

;(def training-data
;  (partition 2
;   (read-string (str "[" (slurp "training_data") "]"))
;   ))

; reference: http://neuralnetworksanddeeplearning.com/chap2.html
; out = a3
; a3 = s(z2)  z2 = b2+w2*a2
; a2 = s(z1)  z1 = b1+w1*a1
; a1 = s(z0)  z0 = b0+w0*a0
; a0 = in
; s: sigmoid function, s(z) = 1/(1+exp(-z))
; s'(z) = exp(-z)/(1+exp(-z))^2
; exps: vector of exp(-z)
; sigs: vector of 1/(1+exp(-z)) = s(z)

; niv : neuron input vector
; nov : neuron output vector
; ndv : neuron output derivative vector
; psv : propagated slope vector
; wm  : weight matrix
; wam : weight matrix to be accumulated
; bv  : intercept vector
; bav : intercept vector to be accumulated
; eov : expected output vector
; edv : error derivative vector

(defn fw1 [{wm :wm bv :bv} niv]
  (let [weighed (map #(+ (reduce + (map * %1 niv)) %2) wm bv)
        expv (map #(Math/exp (- %)) weighed)
        nov (map #(/ (+ 1.0 %)) expv)]
    {:nov nov :ndv (map #(* %1 %2 %2) expv nov)}))

(defn bw1 [{wm :wm bm :bm} niv psv {wam :wam bav :bav}]
  {:wam (map (fn [wrow ps] (map (fn [wel ni] (+ wel (* ni ps)))
                                wrow niv))
             wm psv)
   :bav (map + bav psv)
   :psv (reduce #(map + %1 %2)
                (map (fn [wrow ps] (map #(* % ps)) wrow)
                     wm psv))})

(defn fw [wbs niv]
  (loop [wbs wbs nivs [niv] ndvs []]
    (if (empty? wbs)
      {:nivs nivs :ndvs ndvs}
      (let [{nov :nov ndv :ndv} (fw1 (first wbs) (last nivs))]
        (recur (next wbs) (conj nivs nov) (conj ndvs ndv))
        ))))

(defn bw [wbs {nivs :nivs ndvs :ndvs} eov wbas speed]
  (loop [wbs wbs nivs (butlast nivs) ndvs ndvs wbas wbas
         psv (map #(* (- %1 %2) speed) eov (last nivs)) acc []]
    (if (empty? wbs)
      acc
      (let [{wam :wam bav :bav psv :psv}
            (bw1 (last wbs) (last nivs) psv (last wbas))]
        (recur (butlast wbs) (butlast nivs) (butlast ndvs) (butlast wbas)
               psv (conj acc {:wam wam :bav bav})
               )))))

(defn init-wbas [wbs]
  (vec (map (fn [{wm :wm bv :bv}]
              {:wm (vec (repeat (count wm)
                                (vec (repeat (count (first wm))
                                             0.0))))
               :bv (vec (repeat (count bv) 0.0))})
            wbs)))

(defn learn1 [wbs training-data speed]
  (loop [td training-data wbas (init-wbas wbs)]
    (if (empty? td)
      (r+ wbas)
      (recur (next td)
             (bw wbs (fw wbs (:niv (first td)))
                 (:eov (first td)) wbas speed
                 )))))

(defn init-wbs [n-neuron-vector]
  (vec (map (fn [wcolc bc]
              {:wm (vec (repeat bc 
                                (vec (repeat wcolc 0.0))))
               :bv (vec (repeat bc 0.0))})
            n-neuron-vector
            (next n-neuron-vector))))

(defn calc-output [weight-array b-array invec]
  (loop [ws weight-array, bs b-array, out [invec], deriv []]
    (if (or (empty? ws) (empty? bs))
      [out deriv]
      (let [weighted (map + (m*v (first ws) (last out)) (first bs))
            exps (map #(Math/exp (- %)) weighted)
            sigs (map #(/ (+ 1.0 %)) exps)]
        (recur (next ws) (next bs) (conj out sigs)
               (conj deriv (map #(* %1 %2 %2) exps sigs)) ; s'(z)
               )))))

(defn calc-coeff-deriv [weight-array deriv-array out-array expc scale]
  (loop [ws weight-array
         ds (butlast deriv-array)
         os (butlast out-array)
         delta (map #(* (- %1 %2) %3 scale)
                    (last out-array) expc (last deriv-array))
         acc-w []
         acc-b []]
    (let [deriv (cv*rv delta (last os))]
      (if (or (empty? ds) (empty? os))
        [(cons deriv acc-w) (cons delta acc-b)]
        (recur (butlast ws) (butlast ds) (butlast os)
               (map * (m*v (transpose (last ws)) delta) (last ds))
               (cons deriv acc-w) (cons delta acc-b)
               )))))
