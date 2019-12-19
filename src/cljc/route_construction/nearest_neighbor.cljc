(ns route-construction.nearest-neighbor
  (:require [shared.utils :as utils]
            [shared.common :as common]))

; e (utils/safe-min-key #(common/distance* coords % (last r)) ps)

; (defn make-route [{:keys [coords subroutes-amount]} p]
;   (let [points-set (->> coords keys (remove #{"start" "base"}) set)
;         points-amount (count points-set)]
;     (loop [ps points-set
;            i 0
;            j 0
;            r ["start"]]
;       (if (empty? ps)
;         r
;         (let [[fst snd] (sort-by #(common/distance* coords % (last r)) ps)
;               ; with the shortest having probability 2/3 of being chosen
;               e (if (and snd (> (Math/random) p)) snd fst)
;               ps' (disj ps e)
;               r' (conj r e)
;               i' (inc i)]
;           (if (and (pos? points-amount)
;                    (pos? subroutes-amount)
;                    (>= (/ i' points-amount) (/ (inc j) subroutes-amount)))
;             (recur ps' i' (inc j) (conj r' "base"))
;             (recur ps' i' j       r')))))))

; (defn make-routes [n state]
;   (->> (range n)
;        ; with the shortest having probability 2/3 of being chosen
;        ; for i=0 use honest nearest-neighbor, then choose second in 1/3 cases: random > 0.66
;        (map (fn [i] (make-route state (if (zero? i) 1 0.66))))))

(defn make-route [{:keys [coords subroutes-amount]} i]
  (let [points-set (->> coords keys (remove #{"start" "base"}) set)
        points (->> points-set (sort-by #(common/distance* coords % "start")) vec)
        points-amount (count points-set)
        first-order-index (rem i points-amount)]
    (loop [ps points-set
           i 0
           j 0
           r ["start"]]
      (if (empty? ps)
        r
        (let [e (if (= 1 (count r))
                  (points first-order-index)
                  (utils/safe-min-key #(common/distance* coords % (last r)) ps))
              ps' (disj ps e)
              r' (conj r e)
              i' (inc i)]
          (if (and (pos? points-amount)
                   (pos? subroutes-amount)
                   (>= (/ i' points-amount) (/ (inc j) subroutes-amount)))
            (recur ps' i' (inc j) (conj r' "base"))
            (recur ps' i' j       r')))))))

(defn make-routes [n state]
  (->> (range n)
       (map (fn [i] (make-route state i)))))
