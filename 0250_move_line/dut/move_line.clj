(ns move-line)

(defn xy<->q [[q q-other & els] axes]
  (if (= axes :x)
    (apply vector q q-other els)
    (apply vector q-other q els)))

(defn find-line-end [field [q q-other :as qqo] axes dir]
  (let [[x y] (xy<->q qqo)
        is-end? (case [axes dir]
                  [:x :+] #(= 0 (get-in field [% y 3]))
                  [:x :-] #(= 0 (get-in field [% y 2]))
                  [:y :+] #(= 0 (get-in field [x % 1]))
                  [:y :-] #(= 0 (get-in field [x % 0])))
        qs (case [axes dir]
             [:x :+] (range x (count (first field)))
             [:x :-] (range x -1 -1)
             [:y :+] (range y (count field))
             [:y :-] (range y -1 -1))]
    (first (filter is-end? qs))))

(defn tracked? [track [q q-other :as qqo] axes]
  (not= [0 0] ((if (= axes :x) #(drop 2 %) #(take 2 %))
               (get-in track (xy<->q qqo))
               )))

(defn track-line [track q0 q1 q-other axes]
  (reduce #(-> %1
               (assoc-in (xy<->q [%2 q-other (if (= axes :y) 3 1)] axes) 1)
               (assoc-in (xy<->q [%2 q-other (if (= axes :y) 2 0)] axes) 1))
          track (range q0 q1)))

(defn find-dots [field q0 q1 q-other axes]
  (filter #(= 1 (get-in field (xy<->q [% q-other 4] axes)))
          (range q0 (inc q1))))

(defn track-net [field [x y] axes]
  (loop [track (reduce #(repeat %1 %2)
                       (repeat 7 0)
                       [(count (first field)) (count field)])
         [[q q-other axes :as l1] left] [(xy<->q [x y axes] axes)]]
    (cond (empty? l1) track
          (tracked? track [q q-other] axes) (recur track left)
          :else (let [[q0 q1] (map #(find-line-end field [x y] axes %) 
                                   [:- :+])]
                  (recur
                   (track-line track q0 q1 q-other :x)
                   (concat (map (fn [q] [q-other q (if (= axes :x) :y :x)])
                                (concat [q0 q1]
                                        (find-dots field (inc q0) (dec q1)
                                                   q-other axes)))
                           left))))))
