(ns move-line)

(defn find-line-end [field [x y] axes dir]
  (let [is-end? (case [axes dir]
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

(defn tracked? [track [x y] axes]
  (not= [0 0] ((if (= axes :x) #(drop 2 %) #(take 2 %))
               (get-in track [x y]))))

(defn track-line-x [track x0 x1 y]
  (reduce #(-> %1
               (assoc-in [     %2  y 3] 1)
               (assoc-in [(inc %2) y 2] 1))
          track (range x0 x1)))

(defn track-line-y [track x y0 y1]
  (reduce #(-> %1
               (assoc-in [x      %2  1] 1)
               (assoc-in [x (inc %2) 0] 1))
          track (range y0 y1)))

(defn find-dot-x [field x0 x1 y]
  (filter #(= 1 (get-in field [% y 4]))
          (range x0 x1)))

(defn find-dot-y [field x y0 y1]
  (filter #(= 1 (get-in field [x % 4]))
          (range y0 y1)))

(defn track-net [field [x y] axes]
  (loop [track (reduce #(repeat %1 %2)
                       (repeat 7 0)
                       [(count (first field)) (count field)])
         [[x y axes :as l1] left] [[x y axes]]]
    (cond (empty? l1) track
          (tracked? track [x y] axes) (recur track left)
          :else (let [[q0 q1] (map #(find-line-end field [x y] axes %) 
                                   [:- :+])]
                  (recur (if (= axes :x)
                           (track-line-x track q0 q1 y)
                           (track-line-y track x q0 q1))
                         (concat (map (if (= axes :x)
                                        (fn [x] [x y :y])
                                        (fn [y] [x y :x]))
                                      (if (= axes :x)
                                        (find-dot-x field q0 q1 y)
                                        (find-dot-y field x q0 q1)))
                                 left)))))) 
