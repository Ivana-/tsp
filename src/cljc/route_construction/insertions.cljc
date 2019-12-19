(ns route-construction.insertions
  (:require [shared.utils :as utils]
            [shared.common :as common]))

(defn make-route [{:keys [start-point base-point points]}]
  (loop [s (set points)
         r [start-point base-point]]
    (if (empty? s)
      r
      (let [e (utils/safe-max-key (fn [p] (utils/safe-min-key #(common/distance % p) r)) s)
            [i j] (->> (-> r count range)
                       (partition 2 1)
                       (utils/safe-min-key (fn [[i j]] (+ (common/distance (r i) e) (common/distance (r j) e)))))
            r' (into (conj (subvec r 0 j) e) (subvec r j))]
        (recur (disj s e) r')))))



; ????????????
(defn insertions-2-start-tour [{:keys [start-point base-point points]}]
  (loop [s (set points)
         r [start-point base-point]]
    (if (empty? s)
      r
      (let [[i j e] (->> (for [[i j] (->> (-> r count range)
                                          (partition 2 1))
                               p s]
                           [i j p])
                         (utils/safe-min-key (fn [[i j p]]
                                         (let [a (common/distance (r i) p)
                                               b (common/distance (r j) p)
                                               c (common/distance (r i) (r j))]
                                           (/ (+ (* a a) (* b b) (- (* c c))) (* 2 a b))))))
            r' (into (conj (subvec r 0 j) e) (subvec r j))]
        (recur (disj s e) r')))))


(defn make-routes [n state]
  (->> (range n)
       (map (fn [_] (make-route state)))))