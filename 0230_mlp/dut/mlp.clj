(ns mlp)

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

(defn bw1 [{wm :wm} niv psv {wam :wm bav :bv}]
  {:wm  (map (fn [wrow ps] (map (fn [wel ni] (+ wel (* ni ps)))
                                wrow niv))
             wam psv)
   :bv  (map + bav psv)
   :psv (reduce #(map + %1 %2)
                (map (fn [wrow ps] (map #(* % ps) wrow))
                     wm psv))})

(defn fw [wbs niv]
  (loop [wbs wbs nivs [niv] ndvs []]
    (if (empty? wbs)
      {:nivs nivs :ndvs ndvs}
      (let [{nov :nov ndv :ndv} (fw1 (first wbs) (last nivs))]
        (recur (next wbs) (conj nivs nov) (conj ndvs ndv))
        ))))

(defn bw [wbs {nivs :nivs ndvs :ndvs} eov wbas speed]
  (loop [psv (map #(* (- %1 %2) speed) eov (last nivs))
         wbs wbs nivs (butlast nivs) ndvs ndvs wbas wbas acc []]
    (if (empty? wbs)
      acc
      (let [{wam :wm bav :bv psv :psv}
            (bw1 (last wbs) (last nivs) psv (last wbas))]
        (recur psv (butlast wbs) (butlast nivs) (butlast ndvs) (butlast wbas)
               (cons {:wm wam :bv bav} acc)
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
      (map (fn [wb wba]
             {:wm (map #(map + %1 %2) (:wm wb) (:wm wba))
              :bv (map + (:bv wb) (:bv wba))})
           wbs wbas)
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

(defn calc-err [wbs training-data]
  (loop [td training-data acc []]
    (if (empty? td)
      {:avg (Math/sqrt (/ (apply + acc) (count acc)))
       :max (Math/sqrt (apply max acc))}
      (let [{nivs :nivs} (fw wbs (:niv (first td)))]
        (recur (next td)
               (concat (map #(let [d (- %1 %2)] (* d d))
                            (last nivs) (:eov (first td)))
                       acc))))))
