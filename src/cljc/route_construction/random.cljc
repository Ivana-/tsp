(ns route-construction.random
  (:require [shared.utils :as utils]
            [shared.common :as common]))

(defn make-route [{:keys [coords subroutes-amount]}]
  (let [points-set (->> coords keys (remove #{"start" "base"}) set)
        points-amount (count points-set)]
    (loop [ps points-set
           i 0
           j 0
           r ["start"]]
      (if (empty? ps)
        r
        (let [e (first (shuffle ps))
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
       (map (fn [_] (make-route state)))))
