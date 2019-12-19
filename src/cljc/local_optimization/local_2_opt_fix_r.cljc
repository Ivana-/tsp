(ns local-optimization.local-2-opt-fix-r
  (:require [shared.utils :as utils]
            [shared.common :as common]))


(defonce neighbors-map (atom nil))

(defn get-ixs-close-than [d v i]
  (let [e-i (zipmap v (range))
        l (get @neighbors-map (v i))
        ls (->> l
                (take-while #(< (second %) d))
                ; (take 1)
                (map first))
        r (map e-i ls)]
    ; (prn [i r])
    ; (prn (count r))
    r))

(defn best-2-opt-fix-r-step [v]
  (let [m (dec (count v))]
    (loop [[[i j] :as ixs] (for [i (range 0 (- m 2))
                                 j ; (range (inc i) m)
                                 (get-ixs-close-than (common/distance (v i) (v (inc i))) v i)]
                             [i j])
           tour nil
           gain 0]
      (if (empty? ixs)
        [tour gain]
        (let [x1 (v i)
              x2 (v (inc i))
              y1 (v j)
              y2 (v (inc j))
              gain' (- (+ (common/distance x1 x2) (common/distance y1 y2))
                       (+ (common/distance x1 y1) (common/distance x2 y2)))
              tour' (when (> (- gain' gain) 1E-10) (if (< i j)
                                                     (utils/swap-range v (inc i) j)
                                                     (utils/swap-range v (inc j) i)))]
          (if (and tour' (common/check-subtours? tour'))
            (recur (rest ixs) tour' gain')
            (recur (rest ixs) tour gain)))))))

(defn f-2-opt-fix-r [v]
  (let [v-without-bases (remove #(#{:base} (:type %)) v)]
    (reset! neighbors-map
            (reduce (fn [acc e] (assoc acc e (->> v-without-bases
                                                  (remove #(= e %))
                                                  (map (fn [x] [x (common/distance e x)]))
                                                  (sort-by second)
                                        ; (take neighbors-count)
                                                  )))
                    nil v-without-bases)))
  ; (prn @neighbors-map)
  ; (prn "------------------------------------------------------------------")
  (loop [tour v]
    (let [[tour' gain] (best-2-opt-fix-r-step tour)]
    ; (prn gain (route-distance tour'))
      (if tour' (recur tour') tour))))
