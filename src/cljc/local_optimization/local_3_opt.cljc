(ns local-optimization.local-3-opt
  (:require [shared.utils :as utils]
            [shared.common :as common]))

(defonce stat-3-opt (atom nil))

(defn swap-by-pattern [v pattern]
  (swap! stat-3-opt update :swap-by-pattern (fnil inc 0))
  (loop [i nil
         pattern pattern
         r (transient [])]
    (if (empty? pattern) (persistent! r)
        (let [[from to] (first pattern)
              d (if (< from to) 1 -1)
              i (or i from)
              r' (conj! r (v i))]
          ; (prn (v i) [from to])
          (if (= i to)
            (recur nil (rest pattern) r')
            (recur (+ i d) pattern r'))))))

(defn best-3-opt-change [i j k v gain]
  (let [m (dec (count v))
        x1 i x2 (inc i)
        y1 j y2 (inc j)
        z1 k z2 (inc k)
        removed-dist (+ (common/distance (v x1) (v x2)) (common/distance (v y1) (v y2)) (common/distance (v z1) (v z2)))]
    (loop [[jmp :as jumpers] [[x1 y1 x2 z1 y2 z2]
                              [x1 y2 z1 x2 y1 z2]
                              [x1 z1 y2 x2 y1 z2]
                              [x1 y2 z1 y1 x2 z2]]
           tour nil
           gain gain]

      (swap! stat-3-opt update :change (fnil inc 0))

      (if (empty? jumpers)
        [tour gain]
        (let [added-dist (->> jmp
                              (map v)
                              (partition 2)
                              (map #(apply common/distance %))
                              (reduce + 0))

              ; _ (prn removed-dist added-dist)

              gain' (- removed-dist added-dist)

              ptr (->> (conj jmp m)
                       (into [0])
                       (partition 2))

              ; _ (prn ptr)

              tour' (when (> (- gain' gain) 1E-10) (swap-by-pattern v ptr))]
          (if (and tour' (common/check-subtours? tour'))
            (recur (rest jumpers) tour' gain')
            (recur (rest jumpers) tour gain)))))))

; [1 2 3 4 5 6]

(defn best-3-opt-step [v]
  (let [m (-> v count dec)]
    (loop [[[i j k] :as ixs] (for [i (range 0 (- m 2))
                                   j (range (inc i) (- m 1))
                                   k (range (inc j) m)]
                               [i j k])
           tour nil
           gain 0]
      ; (prn [i j k])
      (if (empty? ixs)
        [tour gain]
        (let [[tour' gain'] (best-3-opt-change i j k v gain)]
          (if tour'
            (recur (rest ixs) tour' gain')
            (recur (rest ixs) tour gain)))))))

(defn f-3-opt [v]
  (loop [tour v]
    (let [[tour-2 gain-2] [nil 0] ; (best-2-opt-step tour)
          [tour-3 gain-3] (best-3-opt-step tour)]
      ; (prn "------------" gain-3)
      (swap! stat-3-opt update :iterations (fnil inc 0))
      (if (every? zero? [gain-2 gain-3])
        tour
        (recur (if (> gain-2 gain-3) tour-2 tour-3))))))
