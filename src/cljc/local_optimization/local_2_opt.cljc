(ns local-optimization.local-2-opt
  (:require [shared.utils :as utils]
            [shared.common :as common]))


; (defn mutations-2-opt [v]
;   (let [m (dec (count v))]
;     (for [i (range 1 m)
;           j (range (inc i) m)
;           :when (let [x1 (v (dec i))
;                       x2 (v i)
;                       y1 (v j)
;                       y2 (v (inc j))]
;                   (< (+ (common/distance x1 y1) (common/distance x2 y2))
;                      (+ (common/distance x1 x2) (common/distance y1 y2))))]
;       (utils/swap-range v i j))))


; (defn mutations-1-move [v]
;   (let [m (dec (count v))]
;     (for [i (range 1 m)
;           j (range 1 m)
;           :when (not (<= -1 (- j i) 1))] ; cause +-1 node shift equals 2 nodes swap
;       (utils/insert-vec (utils/delete-vec v i) j (v i)))))



; (defn move-segment [v i j d]
;   (let [deleted (subvec v i (inc j))
;         after-delete (into (subvec v 0 i) (subvec v (inc j)))
;         k (+ i d)]
;     (-> (subvec after-delete 0 k)
;         (into deleted)
;         (into (subvec after-delete k)))))

; ; (move-segment [0 1 2 3 4 5 6] 1 1 1)
; ; (move-segment [0 1 2 3 4 5 6] 1 3 1)
; ; (move-segment [0 1 2 3 4 5 6] 2 3 -1)

; (defn move-n [n v]
;   (let [m (- (count v) n)]
;     (for [i (range 1 m)
;           d (range (- 1 i) (- m i))
;           :when (not (zero? d))] ; cause +-1 node shift equals 2 nodes swap
;       (move-segment v i (+ i (dec n)) d))))

; ; (move-n 3 [0 1 2 3 4 5 6])

; (defn mutations-123-move [v]
;   (->> (range 1 4)
;        (mapcat #(move-n % v))
;        distinct))



; (defn f-2-opt-1 [v]
;   (loop [way v
;          cst (route-distance v)]
;     (let [r (safe-min-key route-distance (mutations-2-opt way))
;           c (route-distance r)]
;       ; (prn (-> (- cst c) (/ cst) (* 100) int (str " %")))
;       (if (< c cst) (recur r c) way))))

; (defn f-2-opt--- [v]
;   (loop [way v
;          cst (common/route-distance v)
;          cnt 0]
;     (let [t (utils/safe-min-key common/route-distance (mutations-2-opt way))
;           r (if (and t (< (common/route-distance t) cst))
;               t
;               (utils/safe-min-key common/route-distance (mutations-1-move way)))
;           c (common/route-distance r)]
;       ; (prn (-> (- cst c) (/ cst) (* 100) int (str " %")))
;       (if (< c cst) (recur r c (inc cnt)) (do (prn cnt) way)))))


(defn best-2-opt-step [{:keys [settings coords]} v]
  (let [m (dec (count v))]
    (loop [i 1
           j (inc i)
           tour nil
           gain 0]
      (cond (= i m) [tour gain]
            (= j m) (recur (inc i) (+ 2 i) tour gain)
            :else (let [x1 (v (dec i))
                        x2 (v i)
                        y1 (v j)
                        y2 (v (inc j))
                        gain' (- (+ (common/distance* coords x1 x2) (common/distance* coords y1 y2))
                                 (+ (common/distance* coords x1 y1) (common/distance* coords x2 y2)))
                        tour' (when (> gain' gain) (utils/swap-range v i j))]
                    (if (and tour' (common/check-subtours? settings tour'))
                      (recur i (inc j) tour' gain')
                      (recur i (inc j) tour gain)))))))

(defn best-route [state v]
  (loop [tour v]
    (let [[tour' gain] (best-2-opt-step state tour)] (if tour' (recur tour') tour))))
