(ns route-construction.multi-fragment
  (:require [shared.utils :as utils]
            [shared.common :as common]))

(defn make-route [{:keys [coords subroutes-amount]}]

  (prn "make-route")

  (let [;points (into [start-point] (conj points base-point)) ; "abcd"
        points (->> coords keys (remove #{"start" "base"}) vec)
        points-amount (count points)
        edges (->> (loop [[h & t :as points] points
                          r []]
                     (if (empty? points)
                       r
                       (recur t (into r (map (fn [x] [(common/distance* coords h x) h x]) t)))))
                   (sort-by first))
        fragment-index (fn [vertex fragments]
                         (loop [[fragment :as fragments] fragments
                                i 0]
                           (if (empty? fragments)
                             nil
                             (let [c (count (get fragment vertex))]
                               (case c
                                 0 (recur (rest fragments) (inc i))
                                 1 i
                                 :unavailable-edge)))))]
    ;(prn edges)
    (loop [; [[_ a b] & t :as edges] edges
           edges edges
           cnt 0
           r []]
      (if (empty? edges)
        (let [;_ (prn "=====================================================")
              ;_ (prn "r = " r)
              m (first r)
              [f [t1 t2]] (first m)
              z (loop [s #{f t1}
                       i 2
                       j 0
                       r ["start" f t1]]
                  (if (>= i #_(count r) points-amount)
                    r
                    (let [[c p] (->> r reverse (remove #{"base"}) (take 2))
                          n (->> (m c)
                                 (remove #(= p %))
                                 first)
                          r' (conj r n)
                          i' (inc i)]
                      (if (and (pos? points-amount)
                               (pos? subroutes-amount)
                               (>= (/ i' points-amount) (/ (inc j) subroutes-amount)))
                        (recur (conj s n) i' (inc j) (conj r' "base"))
                        (recur (conj s n) i' j       r')))))]
          ; (into [start-point] (conj z base-point))
          ; (conj z (first z))
          z)

        (let [; with the shortest having probability 2/3 of being chosen
              [e1 e2 & es] edges
              [[_ a b] t] (if (and e2 (> (Math/random) 0.3 #_0.66))
                            [e2 (cons e1 es)]
                            [e1 (rest edges)])

              add-ab #(-> %
                          (update a (fnil conj []) b)
                          (update b (fnil conj []) a))
              ia (fragment-index a r)
              ib (fragment-index b r)]
          ; (prn "-------------------------------------------------------------------")
          ; (prn a b cnt r)
          (cond
            (some #(= :unavailable-edge %) [ia ib]) (recur t cnt r)
            (and ia ib (= ia ib)) (if (< cnt (dec points-amount))
                                    (recur t cnt r)
                                    (recur t (inc cnt) (update r ia add-ab)))
            (= nil ia ib) (recur t (inc cnt) (conj r (add-ab nil)))
            (and ia ib) (recur t (inc cnt) (-> r
                                               (assoc ia (add-ab (merge (r ia) (r ib))))
                                               (utils/delete-vec ib)))
            ia (recur t (inc cnt) (update r ia add-ab))
            ib (recur t (inc cnt) (update r ib add-ab))
            ; :else
            ))))))

(defn make-routes [n state]
  (->> (range n)
       (map (fn [_] (make-route state)))))
