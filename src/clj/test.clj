(ns test)

; http://tsp-basics.blogspot.com/2017/03/3-opt-move.html
; http://www-e.uni-magdeburg.de/mertens/TSP/node2.html#sec:nearestneighbor
; https://cs.stackexchange.com/questions/19808/how-does-the-3-opt-algorithm-for-tsp-work
; https://www.cs.ubc.ca/~hutter/previous-earg/EmpAlgReadingGroup/TSP-JohMcg97.pdf


(defn rand-from-to [from to] (+ from (int (* (- to from -1) (Math/random)))))

(rand-from-to 1 2)

(macroexpand '(time (+ 1 2)))

; (apply min-key first [])

(defn swap-vec [v i j] (assoc v i (v j) j (v i)))


;;;;;;;;;; swap sycle diap!!!!

; (defn norm-v [n v] (mod n (count v)))

; (defn swap-range [v i j]
;   (let [norm #(norm-v % v)] ; (mod -1 3) => 2, (rem -1 3) => -1
;     (loop [a (norm (inc i))
;            b j
;            iter-cnt (quot (norm (- j i)) 2)
;            r v]
;       ; (prn a b iter-cnt)
;       (if (<= iter-cnt 0)
;         r
;         (recur (norm (inc a)) (norm (dec b)) (dec iter-cnt) (swap-vec r a b))))))

; (defn mutations-2-opt [v]
;   (let [rng (range (count v))]
;     (for [i rng
;           j rng
;           :when (pos? (quot (norm-v (- j i) v) 2))]
;       ; (swap-range v i j)
;       [i j]
      
;       )))

; (mutations-2-opt ["a" "b" "c"])

(defn swap-range
  "swap elements order from index i to j included both!"
  [v i j]
  (let [a (min i j)
        b (max i j)]
    (loop [c a
           r (transient v)]
      (if (> c b)
        (persistent! r)
        (recur (inc c) (assoc! r c (v (+ a (- b c)))))))))

(defn swap-by-pattern [v pattern]
  (loop [i nil
         pattern pattern
         r (transient [])]
    (if (empty? pattern) (persistent! r)
        (let [[from to] (first pattern)
              d (if (< from to) 1 -1)
              i (or i from)
              r' (conj! r (v i))]
          (prn (v i) [from to])
          (if (= i to)
            (recur nil (rest pattern) r')
            (recur (+ i d) pattern r'))))))

(defn best-3-opt-step [v]
  (let [m (dec (count v))]
    (loop [i 0
           j (inc i)
           k (inc j)
           acc []]
      (cond (>= i (- m 2)) acc
            (>= j (- m 1)) (recur (inc i) (+ 2 i) (+ 3 j) acc)
            (>= k m) (recur i (inc j) (+ 2 j) acc)
            :else (recur i j (inc k) (conj acc [i j k]))))))

(defn delete-vec [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(delete-vec [0 1 2 3 4] 2)

(comment
  (swap-by-pattern
   (vec #_"abcdefgh" (range 6))
  ;  [[0 2] [4 3] [5 5]]
   [[5 4] [0 3]])
  (partition 2 '[x1 y1 x2 z1 y2 z2])

  (->> (conj '[x1 y1 x2 z1 y2 z2] 9)
       (into [0])
       (partition 2))

  (let [r [{:a 1 :b 2} {:c 3}]]
    (some #(when-let [val (% :c)] [val %]) r))

  (->>
   (let [v (-> 23 range vec)
         m (-> v count dec)]
     (for [i (range 0 (- m 2))
           j (range (inc i) (- m 1))
           k (range (inc j) m)]
       [i j k]))
   count)

  (->> (range 10)
       (partition 2 1)
       (map vec))

  (let [[h & ps] [0 1 2 3]] {:h h :t ps})
  (update [0 1 2] 1 inc)
  (assoc [0 1 2] 1 9)

  (let [a 1 b 1] (set [a b]))
  (= nil nil nil)
  (get [0 1 2 3] 1)
  (get nil "sdfgsd")

  (let [v [0 1 2 3]]
    (loop [[h & t :as v] v
           r nil]
      (if (empty? v) r (recur t (into r (map (fn [x] [h x]) t))))))

  (zipmap "dfcz" (range))

  (->> [0 0.0 0.0 0 0.0 0] (map hash) (apply =))
  (hash 0)

  (loop [[[i j] :as ixs] (->> (for [i (range)
                                    j (range)
                                    :when (< i j)]
                                [i j])
                              (take 5))]
    (if (empty? ixs) 33 (do (prn i j) (recur (rest ixs)))))

  (every? zero? [0 0 0.0])

  (best-3-opt-step [0 1 2 3 4 5])
  (+ 1 2 3)
;
  )

(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))


(defn lk-steps [l n]
  (if (= l 6)
    [l n]
    (let [ij (for [i [(dec n) (inc n)]
                   j [(* 10 i)]]
               (+ i j))]
      (->> (map #(lk-steps (inc l) %) ij)
          ;  (some identity)
           ))))

(comment

  (lk-steps 0 10)

  (let [v [0 1 2 3 4 5]
        m (dec (count v))]
    (for [i (range 1 m)
          j (range (inc i) m)]
      [i j]))

  (defn mutations-2-opt [v]
    (let [m (dec (count v))]
      (for [i (range 1 m)
            j (range (inc i) m)]
        (swap-range v i j))))

  (let [v (vec (range 20))
        m (mutations-2-opt v)
        mm (mapcat mutations-2-opt m)]
    [(count m) (count (distinct (into m mm)))])

; 20 [153 23562]
; 20 + distinct [153 15611]

  (swap-vec  [0 1 2 3 4 5] 2 4)
  (swap-vec  [0 1 2 3 4 5] 1 3)
  (swap-range [0 1 2 3 4 5] 2 4)
  (swap-range [0 1 2 3 4 5] 4 2)
  (swap-range [0 1 2 3 4 5] 0 3)
  (swap-range [0 1 2 3 4 5] 3 0)
  (swap-range [0 1 2 3 4 5] 2 3)
  (swap-range [0 1 2 3 4 5] 3 2)

  (subvec [0 1 2 3 4 5 6 7] 2 4)
  (subvec [0 1 2 3 4 5 6 7] 0 0)
;
  )

(defn exclude [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn insert [v i e]
  (into (conj (subvec v 0 i) e) (subvec v i)))

(exclude [0 1 2 3 4 5] 0)
(exclude [0 1 2 3 4 5] 5)
(map #(insert [0 1 2 3] % 'A) (range 5))
; ([A 0 1 2 3] [0 A 1 2 3] [0 1 A 2 3] [0 1 2 A 3] [0 1 2 3 A])


(let [x (let [v [0 1 2 3 4 5]
              m (dec (count v))]
          (for [i (range 1 m)
                j (range 1 m)
                :when (not (<= 0 (- i j) 1))]
            (insert (exclude v i) j (v i)))) ;
      ]
  (= x (distinct x)))


(replicate 3 1) ; (1 1 1)

(defn rand-n [n] (int (* n (Math/random))))

(int 9.95)

(rand-n 10)

; (defn swap-vec [v i j] (assoc v i (v j) j (v i)))

(defn mutation [v]
  (let [n (count v)]
    (swap-vec v (rand-n n) (rand-n n))))

; (1,2,3,4,5,6,7).
; Генератор случайных чисел выбрал города 2 и 7, мы выполнили процедуру и получили 
; (1,7,6,5,4,3,2).

(defn mutation-1 [v]
  (let [n (count v)]
    (swap-range v (rand-n n) (rand-n n))))

(mutation-1 [1 2 3 4 5 6 7])

(map (fn [_] (mutation [0 1 2 3 4 5])) (range 3))


(concat [1 2 3] [4 5 6] [7 8])

(def v [1 2 3])

(concat v
        (for [e v
              i (range 3)]
          (+ i (* 10 e))))

(->> [0 1 2 3 4 5]
     (partition 2 1)
     (map (fn [[x y]] (- y x)))
     (reduce + 0))


(defn distance [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn cost [e]
  (->> e
       (partition 2 1)
       (map distance)
       (reduce + 0)))

(defn greedy-tour [v]
  (let [open-tour (loop [s (set v)
                         r []]
                    (if (empty? s)
                      r
                      (let [e (if (empty? r)
                                (first v)
                                (apply min-key (fn [x] (distance [x (last r)])) s))]
                        (recur (disj s e) (conj r e)))))
        [a _] (->> (conj open-tour (first open-tour))
                   (partition 2 1)
                   (apply max-key distance))]
    (->> open-tour
         cycle
         (drop-while #(not= % a))
         rest
         (take (count open-tour))
         vec)))

(def s #{1 2 3 4})

(defn dst [[x1 x2]] (let [dx (- x2 x1)] (if (neg? dx) (- dx) dx)))

(defn cs [e]
  (->> e
       (partition 2 1)
       (map dst)
       (reduce + 0)))

; (cs [1 2 3 4])
; (cs [1 3 2 4])

(def state (atom {:goal 100
                  :tour nil}))

(defn foo [s r]
  (let [goal (cs r)]
    (cond
      (empty? s) (when (< goal (:goal @state))
                   (swap! state assoc :goal goal :tour r))
      (> goal (:goal @state)) nil
      :else (doseq [e s]
              (foo (disj s e) (conj r e))))))

(set (-> [2 2 2 2 2] count range))

(comment
  (foo s [])
  (:tour @state)
;
  )
; 3
; (->> [1 2 3 4]
;      cycle
;      (drop-while #(not= % 3))
;      rest
;      (take 4))


(let [v [1 2 3 4]]
  (for [a v
        b v
        :when (not= a b)]
    [a b]))

(defn ordered-pairs [v]
  (if (empty? v) []
      (concat
       (map (fn [x] [(first v) x]) (rest v))
       (ordered-pairs (rest v)))))

(defn partial-sums [v]
  (:result (reduce (fn [acc x]
                     (let [r (+ x (:sum acc))]
                       (-> acc
                           (update :result conj r)
                           (assoc :sum r))))
                   {:result [] :sum 0} v)))

(partial-sums [1 1 1 1 1])

(ordered-pairs [1 2 3 4])

(defn f-2-opt [v]
  (let [changing-pairs (-> v count range ordered-pairs)
        vs (map (fn [[i j]] (swap-vec v i j)) changing-pairs)]
    vs))

(f-2-opt ["a" "b" "c" "d"])


; (defn partial-sums [v]
;   (:result (reduce (fn [acc x]
;                      (let [r (+ x (:sum acc))]
;                        (-> acc
;                            (update :result conj r)
;                            (assoc :sum r))))
;                    {:result [] :sum 0} v)))


(cost [[1 1] [3 5] [2 2] [7 6]])
(cost [[1 1] [2 2] [3 5] [7 6]])
(mutation [[1 1] [2 2] [3 5] [7 6]])

(defn update-ga [data]
  (let [v data ; (take 10 data)
        new-gen (concat v
                        (for [e v
                              i (range 10)]
                          (mutation e)))
        z (->> new-gen
               distinct
               (map (fn [e] [(cost e) e]))
               (sort-by first)
            ;    (map second)
               (take 10))]
    z))


(update-ga [[[1 1] [3 5] [2 2] [7 6]]])

(into '(1 2 3) '(4))
(into [1 2 3] [4 5 6])


(partition-all 3 [0 1 2 3 4 5 6 7])

(butlast [0 1 2 3 4 5 6 7])
(last [0 1 2 3 4 5 6 7])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2-opt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; ; (defn mutate-2-opt [v i j] (swap-vec v i j))
; (defn mutate-2-opt [v i j] (swap-range v i j))

; (defn f-2-opt [v]
;   (loop [way v
;          cst (cost v)]
;     ; (prn cst)
;     (let [changing-pairs (-> way count range ordered-pairs)
;           vs (map (fn [[i j]] (mutate-2-opt way i j)) changing-pairs)
;           r (apply min-key cost vs)
;           c (cost r)]
;       (if (< c cst) (recur r c) way))))


; (defn norm-v [n v] (mod n (count v)))

; (defn swap-range [v i j]
;   (let [norm #(norm-v % v)] ; (mod -1 3) => 2, (rem -1 3) => -1
;     (loop [a (norm (inc i))
;            b j
;            iter-cnt (quot (norm (- j i)) 2)
;            r v]
;       ; (prn a b iter-cnt)
;       (if (<= iter-cnt 0)
;         r
;         (recur (norm (inc a)) (norm (dec b)) (dec iter-cnt) (swap-vec r a b))))))

; (defn mutations-2-opt [v]
;   (let [rng (range (count v))]
;     (for [i rng
;           j rng
;           :when (pos? (quot (norm-v (- j i) v) 2))]
;       (swap-range v i j))))

; (defn f-2-opt [v]
;   (loop [way v
;          cst (cost v)]
;     (let [r (apply min-key cost (mutations-2-opt way))
;           c (cost r)]

;       ; (prn "cst" (mutations-2-opt way))

;       (if (< c cst) (recur r c) way))))

; (defn improve []
;   (let [{:keys [route goal]} @state
;         improved-tour (f-2-opt route)
;         improved-tour-cost (cost improved-tour)]
;     (prn (-> (- goal improved-tour-cost)
;              (/ goal)
;              (* 100)
;              int
;              (str " %")))
;     (swap! state assoc
;            :goal improved-tour-cost
;            :route improved-tour)))

; (defn greedy-first [data]
;   (f-2-opt (greedy-tour data 0)))

; (defn greedy-best [data]
;   (->> data
;        count
;        range
;        (map #(f-2-opt (greedy-tour data %)))
;        (apply min-key cost)
;       ;  (map #(greedy-tour data %))
;       ;  (apply min-key cost)
;       ;  f-2-opt
;        ))


; (defonce closest-neighbors (atom nil))

; (defn get-closest-neighbors [neighbors-count v]
;   (reduce (fn [acc e] (assoc acc e (->> v
;                                         (remove #(= e %))
;                                         (sort-by (fn [x] (distance [e x])))
;                                         (take neighbors-count))))

;           nil v))

; (defn mutations-2-opt [v]
;   (let [m (dec (count v))]
;     (for [i (range 1 m)

;           j (range (inc i) m)

;           ; j (let [jj (->> (v i)
;           ;                 (get @closest-neighbors)
;           ;                 (map #(.indexOf v %))
;           ;                 (filter #(< i % m)))]
;           ;     ; (prn i jj)
;           ;     jj)
;           :when (let [x1 (v (dec i))
;                       x2 (v i)
;                       y1 (v j)
;                       y2 (v (inc j))]
;                   (< (+ (distance [x1 y1]) (distance [x2 y2]))
;                      (+ (distance [x1 x2]) (distance [y1 y2]))))]
;       (swap-range v i j))))



; (defonce distances-cash (atom nil))

; (defn distance [[t1 t2]]
;     ; p = 0.017453292519943295     #Pi/180
;     ; a = 0.5 - cos((lat2 - lat1) * p)/2
;     ;         + cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p)) / 2
;     ; return 12742 * asin(sqrt(a)) #2*R*asin...
;   ; (let [distances-cash-key (str (:id t1) "-" (:id t2))] ; [t1 t2])
;   ;   (or (get @distances-cash distances-cash-key)
;   (let [[lat1 lon1] (get-lat-lng t1)
;         [lat2 lon2] (get-lat-lng t2)
;         p 0.017453292519943295
;         a1 (* 0.5 (Math/cos (* p (- lat2 lat1))))
;         a2 (* 0.5
;               (Math/cos (* p lat1))
;               (Math/cos (* p lat2))
;               (- 1 (Math/cos (* p (- lon2 lon1)))))
;         a (+ 0.5 (- a1) a2)
;         r (* 12742 (Math/asin (Math/sqrt a)))]
;           ; (swap! distances-cash assoc distances-cash-key r)
;     r)) ; ))

; (dмесяц может быть вообще конец светаЮb) r (recur (inc a) (dec b) (assoc r a (v b) b (v a))))))


; (comment

;   (defn reverse-diap [vec from to]
;     (mapv #(nth vec (dec %)) (range to from -1)))

;   (defn swap-range*
;     "swap elements order from index i to j included both!"
;     [v i j]
;     (if (= i j)
;       v
;       (let [arr (to-array v)]
;         (loop [a (min i j)
;                b (max i j)]
;           (if (>= a b)
;             (vec arr)
;             (let [t (aget arr a)]
;               (aset arr a (aget arr b))
;               (aset arr b t)
;               (recur (inc a) (dec b))))))))

;   (time (let [v (-> 1E3 range vec)]
;           (count #_distinct (for [i (range (count v))
;                                   j (range i (count v))]
;                               ; (= (swap-range v i j) (swap-range* v i j))
;                               (swap-range v i j) ; "Elapsed time: 7156.572399 msecs"
;                               ; (swap-range* v i j) ; "Elapsed time: 8650.717967 msecs"
;                               ))))
;   (let [v [0 1 2 3 4 5]
;         v' (swap-range* v 1 3)]
;     [v v' v])

;   ; (defn example []
;   ;   (let [arr (int-array 10)
;   ;         n 10]
;   ;     (aset arr 0 1)
;   ;     (doseq [i (range (inc n))]
;   ;       (aset arr i 0))
;   ;     (aget arr n)))
;   ;;
;   )

; (defn delete [v i] (into (subvec v 0 i) (subvec v (inc i))))

; (defn insert [v i e] (into (conj (subvec v 0 i) e) (subvec v i)))

; (defn mutations-1-move [v]
;   (let [m (dec (count v))]
;     (for [i (range 1 m)
;           j (range 1 m)
;           :when (not (<= -1 (- j i) 1))] ; cause +-1 node shift equals 2 nodes swap
;       (insert (delete v i) j (v i)))))



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

; (defn mutations-many-move [v]
;   (->> (range 1 2)
;        (mapcat #(move-n % v))
;        distinct))

; ; ; (mutations-123-move [0 1 2 3 4 5 6])

