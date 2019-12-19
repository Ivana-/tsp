(ns shared.common)

(defn distance [{[x1 y1] :coords} {[x2 y2] :coords}]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn route-distance [route]
  (->> route
       (partition 2 1)
       (map #(apply distance %))
       (reduce + 0)))

(defn distance* [coords id-1 id-2]
  (let [[x1 y1] (coords id-1)
        [x2 y2] (coords id-2)
        dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn route-distance* [coords route]
  (->> route
       (partition 2 1)
       (map #(apply (partial distance* coords) %))
       (reduce + 0)))


(defn invalid-subtour? [{:keys [max-points-in-subroute]} subtour]
  ; TODO add real subtour check by max sacks or something else
  (> (count subtour) max-points-in-subroute))

(defn check-subtours? [params v]
  (let [subtours (->> v
                      (partition-by #{"start" "base"})
                      (remove #(-> % first #{"start" "base"})))]
    (not (some #(invalid-subtour? params %) subtours))))

  ; (loop [[{:keys [type]} :as ps] v
  ;        subtour-state nil]
  ;   (cond (empty? ps) true
  ;         (= :base type) (if (> (or (:count subtour-state) 0)
  ;                               (:max-points-in-subroute global-params))
  ;                          false
  ;                          (recur (rest ps) nil))
  ;         (= :start type) (recur (rest ps) subtour-state)
  ;         :else (recur (rest ps) (update subtour-state assoc :count (fnil inc 0))))))


(defn random-point [] [(Math/random) (Math/random)])

(defn random-data [n]
  (->> (range n)
       (map (fn [i] {(str i) (random-point)}))
       (reduce merge
               {"start" (random-point)
                "base" (random-point)})))

(defn random-tour [{:keys [start-point base-point points]}]
  (into [start-point] (conj points base-point)))

