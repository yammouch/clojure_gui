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
; psv : propagated slope vector
; wm  : weight matrix
; wam : weight matrix to be accumulated
; bv  : intercept vector
; bav : intercept vector to be accumulated
; eov : expected output vector

(defn fw1 [{wm :wm bv :bv} niv]
  (let [weighed (map #(+ (reduce + (map * %1 niv)) %2) wm bv)
        expv (map #(Math/exp (- %)) weighed)
        nov (map #(/ (+ 1.0 %)) expv)]
    nov))

(defn bw-wb [niv psv {wam :wm bav :bv}]
  {:wm  (map (fn [wrow ps] (map (fn [wel ni] (+ wel (* ni ps)))
                                wrow niv))
             wam psv)
   :bv  (map + bav psv)})

(defn bw-psv [wm psv niv]
  (map #(* %1 %2 (- 1.0 %2))
       (apply map + (map #(map (partial * %2) %1)
                          wm psv))
       niv))

(defn fw [wbs niv]
  (loop [wbs wbs acc [niv]]
    (if (empty? wbs)
      acc
      (recur (next wbs)
             (conj acc (fw1 (first wbs) (last acc)))
             ))))

(defn bw [wbs nivs eov wbas speed]
  (loop [psv (map #(* (- %1 %2) speed) eov (last nivs)),
         wbs wbs, nivs (butlast nivs), wbas wbas, acc []]
    (let [wb-updated (bw-wb (last nivs) psv (last wbas))]
      (if (empty? (butlast wbas))
        (cons wb-updated acc)
        (recur (bw-psv (:wm (last wbs)) psv (last nivs))
               (butlast wbs) (butlast nivs) (butlast wbas)
               (cons wb-updated acc))))))

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
             {:wm (map #(map + (map (partial * 0.99999) %1) %2)
                       (:wm wb) (:wm wba))
              :bv (map + (:bv wb) (:bv wba))})
           wbs wbas)
      (recur (next td)
             (bw wbs (fw wbs (:niv (first td)))
                 (:eov (first td)) wbas speed
                 )))))

(defn init-wbs
 ([n-neuron-vector] (init-wbs n-neuron-vector (repeat 0.0)))
 ([n-neuron-vector seq]
  (loop [seq seq, [n & ns] n-neuron-vector, acc []]
    (if (empty? ns)
      acc
      (let [[s1 s2] (split-at (* n (first ns)) seq)]
        (recur s2 ns (conj acc {:wm (partition n s1)
                                :bv (repeat (first ns) 0.0)
                                })))))))

(defn calc-err [wbs training-data]
  (loop [td training-data acc []]
    (if (empty? td)
      {:avg (Math/sqrt (/ (apply + acc) (count acc)))
       :max (Math/sqrt (apply max acc))}
      (let [nivs (fw wbs (:niv (first td)))]
        (recur (next td)
               (concat (map #(let [d (- %1 %2)] (* d d))
                            (last nivs) (:eov (first td)))
                       acc))))))
