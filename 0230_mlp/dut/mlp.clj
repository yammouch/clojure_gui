(defn m*v [m v] (map #(reduce + (map * % v)) m)

(def training-data
  (partition 2
   (read-string (str "[" (slurp "training_data") "]"))
   ))

; reference: http://neuralnetworksanddeeplearning.com/chap2.html
; out = s(z^2)
; z^2 = b^2+w^2*s(z^1)
; z^1 = b^1+w^1*s(z^0)
; z^0 = b^0+w^0*s(in)
; s: sigmoid function, s(z) = 1/(1+exp(-z))
; s'(z) = exp(-z)/(1+exp(-z))^2
; exps: vector of exp(-z)
; sigs: vector of 1/(1+exp(-z)) = s(z)

(def calc-out-sigmoid-deriv [weight-layers b-layers input]
  (loop [ws weight-layers, bs b-layers, prev-z input, acc []]
    (let [exps (map #(Math/exp (- %)) prev-z)
          sigs (map #(/ (+ 1.0 %)) exps)
      (if (empty? ws)
        [sigs acc]
        (recur (next ws) (next bs)
               (map + (m*v (first ws) sigs) (first bs))
               (conj acc (map #(* %1 %2 %2) exps sigs))
               )))))
