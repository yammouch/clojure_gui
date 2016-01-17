(ns schemanip)

; {:cmd :move-x :org [4 6] :dst 8}

(defn slide-upper [{field :field cmd :cmd :as x}]
  (if (or (= (get-in cmd [:org 1]) 0) nil
          (and (= (:cmd cmd) :move-y)
               (= (:dst cmd) 0))
          (some (partial some (complement zero?))
                (first (:body field))))
    nil
    (-> (if (= (:cmd cmd) :move-y)
          (update-in x [:cmd :dst] dec)
          x)
        (assoc-in [:field :body]
                  (concat (next (get field :body))
                          [(reduce #(repeat %2 %1) 0
                                   [(count (get-in field [:body 0 0]))
                                    (get-in field [:size 0])])]))
        (update-in [:cmd :org 1] dec))))
