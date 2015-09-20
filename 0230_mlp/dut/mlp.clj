(ns mlp)

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

(defn calc-output [weight-array b-array invec]
  (loop [ws weight-array, bs b-array, out [invec], deriv []]
    (if (or (empty? ws) (empty? bs))
      [out deriv]
      (let [weighted (map + (m*v (first ws) (last out)) (first bs))
            exps (map #(Math/exp %) weighted)
            sigs (map #(/ (+ 1.0 %)) exps)]
        (recur (next ws) (next bs) (conj out sigs)
               (conj deriv (map #(* (- %1) %2 %2) exps sigs)) ; s'(z)
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
