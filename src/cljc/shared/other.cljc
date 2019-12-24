(ns shared.other
  (:require [shared.utils :as utils]
            [clojure.set :as set]))


; (defn mutation [v]
;   (let [n (count v)
;         i (rand-from-to 1 (- n 2))
;         j (rand-from-to 1 (- n 2))]
;     (swap-range v (min i j) (max i j))))

; (defn update-ga [data]
;   (let [v data ; (take 10 data)
;         new-gen (concat v
;                         (for [e v
;                               i (range 5)]
;                           (mutation e)))
;         z (->> new-gen
;                distinct
;                (sort-by route-distance)
;                (take 5)
;                vec
;               ;  (concat (map shuffle v))
;                )]
;     z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (def dp (atom nil))
; (def dv (atom nil))

; ; (def fib-memoize
; ;   (memoize (fn [n]
; ;              (if (< n 2)
; ;                n
; ;                (+ (fib-memoize (- n 1)) (fib-memoize (- n 2)))))))

; (defn foo [p s]
;   (let [memo-val (get @dp [p s])]
;     (cond
;       (empty? s) {:route-distance 0 :tour [p]}
;       memo-val memo-val
;       :else (let [r (->> s
;                          (mapv (fn [q]
;                                  (let [s-q (disj s q)
;                                        {:keys [route-distance tour] :as pr} (foo q s-q)
;                                        r (if p
;                                            {:route-distance (+ route-distance
;                                                               (common/distance (get @dv p) (get @dv q)
;                                           ;  p q
;                                                                         ))
;                                             :tour (into [p] tour)}
;                                            pr)]
;                                 ; (swap! dp assoc [q s-q] r)
;                                    r)))
;                          (utils/safe-min-key :route-distance))]
;               (swap! dp assoc [p s] r)
;               r))))

; ; 9 raw:   "Elapsed time: 2562.930000 msecs" 0
; ; 9 m-not: "Elapsed time: 134003.160000 msecs" 2296
; ; 9 memo:  "Elapsed time: 1689.605000 msecs" 2296

; (defn brut-force [v]
;   (reset! dp nil)
;   (reset! dv v)
;   (prn (count @dp))
;   (let [{:keys [route-distance tour]} (time (foo nil (set (-> v count range))))]
;     (prn (count @dp))
;     ; (prn @dv)
;     ; (prn @dp)
;     {:route-distance route-distance
;      :tour (mapv #(get @dv %) tour)}))

; (defn partial-sums [v]
;   (:result (reduce (fn [acc x]
;                      (let [r (+ x (:sum acc))]
;                        (-> acc
;                            (update :result conj r)
;                            (assoc :sum r))))
;                    {:result [] :sum 0} v)))




; (defn points-view- [main-state]
;   (let [state (r/atom nil)
;         padding-percent 5
;         ; norm (fn [x limit] (* x limit))
;         norm (fn [x limit] (let [p (/ padding-percent 100)]
;                              (+ (* x limit (- 1 p p)) (* limit p))))
;         handle-mouse-move (fn mouse-move [e]
;                             (let [{:keys [dragging-id dx dy area-node area-width area-height]} @state
;                                   area-bounds (.getBoundingClientRect area-node)
;                                   ; x (/ (- (.-clientX e) (.-left area-bounds) (.-clientLeft area-node) dx) area-width)
;                                   ; y (/ (- (.-clientY e) (.-top area-bounds)  (.-clientTop area-node)  dy) area-height)
;                                   offset-x (* 0.01 padding-percent area-width)
;                                   offset-y (* 0.01 padding-percent area-height)
;                                   base-x (- area-width (* 2 offset-x))
;                                   base-y (- area-height (* 2 offset-y))
;                                   x (/ (- (.-clientX e) offset-x (.-left area-bounds) (.-clientLeft area-node) dx) base-x)
;                                   y (/ (- (.-clientY e) offset-y (.-top area-bounds)  (.-clientTop area-node)  dy) base-y)]
;                               (when dragging-id
;                                 (swap! main-state assoc-in [:coords dragging-id] [x y]))))
;         handle-mouse-up  (fn mouse-up [e]
;                            ;;(prn "up")
;                            (swap! state dissoc :dragging-id)
;                            (.removeEventListener js/document "mousemove" handle-mouse-move)
;                            (.removeEventListener js/document "mouseup" mouse-up)
;                            (run main-state))
;         handle-mouse-down  (fn [e]
;                              ;;(prn "down")
;                              (.removeAllRanges (.getSelection js/window))
;                              (let [bounds (-> e .-target .getBoundingClientRect)
;                                    dx (- (.-clientX e) (/ (+ (.-left bounds) (.-right bounds)) 2))
;                                    dy (- (.-clientY e) (/ (+ (.-top bounds) (.-bottom bounds)) 2))]
;                                (swap! state assoc
;                                       :dragging-id (-> e .-currentTarget (.getAttribute "data-id"))
;                                       :dx dx :dy dy)
;                                (.addEventListener js/document "mousemove" handle-mouse-move)
;                                (.addEventListener js/document "mouseup" handle-mouse-up)))
;         handle-window-resize (fn [e] (swap! state assoc :window-resized? true))]

;     (r/create-class
;      {:component-did-mount (fn [this]
;                             ;  (prn "component-did-mount")
;                              (.addEventListener js/window "resize" handle-window-resize)
;                              (swap! state assoc :area-node (r/dom-node this)))

;       :component-will-unmount (fn [this]
;                                 (.removeEventListener js/window "resize" handle-window-resize))

;       :component-did-update (fn [prev-props prev-state]
;                               ; (prn "component-did-update")
;                               (when-let [root (:area-node @state)]
;                                 (let [bounds (.getBoundingClientRect root)]
;                                   (swap! state assoc
;                                          :window-resized? false
;                                          :area-width (- (.-right bounds)  (.-left bounds))
;                                          :area-height (- (.-bottom bounds) (.-top bounds))))))

;       :reagent-render (fn [main-state]
;                         ; (prn "reagent-render")
;                         (let [{:keys [area-width area-height]} @state
;                               norm-x #(norm % area-width)
;                               norm-y #(norm % area-height)
;                               {:keys [coords route]} @main-state]
;                           [:div.svg-container {:style {:width "100%" :height "100%"
;                                                       ;  :overflow :hidden
;                                                        }}
;                            [:svg.field {; :width area-width :height area-height
;                                         :width "100%" :height "100%"
;                                         :style {;:width (str area-width "px") :height (str area-height "px")
;                                                 :background-color :aliceblue}}
;                                         ;;{:viewBox "0 0 1 1"}
;                             (when-not (empty? route)
;                               [:polyline {:points (->> route
;                                                        (map (fn [id]
;                                                               (let [[x y] (coords id)]
;                                                                 (str (norm-x x) "," (norm-y y)))))
;                                                        (str/join " "))
;                                           :fill :none :stroke :black :stroke-width 3}])
;                             (for [[id [x y]] coords]
;                               ^{:key #_id (gensym)}
;                               [:circle (merge {:cx (norm-x x) :cy (norm-y y)
;                                                :r 8 :stroke "#444" :stroke-width 3 :fill :yellow
;                                                :onMouseDown handle-mouse-down
;                                                :zazaza 333
;                                                :data-id id}
;                                               (case id
;                                                 "base"  {:r 12 :stroke-width 0 :fill :brown}
;                                                 "start" {:r 10 :stroke-width 0 :fill :green}
;                                                 nil))])]]))})))


; (defn points-view-core-- [{:keys [render-function on-drag-start on-drag on-drag-end]}]
;   (let [state (r/atom nil)
;         padding-percent 5
;         ; norm (fn [x limit] (* x limit))
;         norm (fn [x limit] (let [p (/ padding-percent 100)]
;                              (+ (* x limit (- 1 p p)) (* limit p))))

;         get-normalized-coords (fn [e]
;                                 (let [{:keys [dx dy area-node area-width area-height]} @state
;                                       area-bounds (.getBoundingClientRect area-node)
;                                       ; x (/ (- (.-clientX e) (.-left area-bounds) (.-clientLeft area-node) dx) area-width)
;                                       ; y (/ (- (.-clientY e) (.-top area-bounds)  (.-clientTop area-node)  dy) area-height)
;                                       offset-x (* 0.01 padding-percent area-width)
;                                       offset-y (* 0.01 padding-percent area-height)
;                                       base-x (- area-width (* 2 offset-x))
;                                       base-y (- area-height (* 2 offset-y))
;                                       x (/ (- (.-clientX e) offset-x (.-left area-bounds) (.-clientLeft area-node) dx) base-x)
;                                       y (/ (- (.-clientY e) offset-y (.-top area-bounds)  (.-clientTop area-node)  dy) base-y)]
;                                   [x y]))

;         handle-mouse-move (fn mouse-move [e]
;                             (when on-drag
;                               (let [{:keys [dragging-id]} @state
;                                     [x y] (get-normalized-coords e)]
;                                 (when dragging-id
;                                   (on-drag dragging-id x y)))))

;         handle-mouse-up  (fn mouse-up [e]
;                            ;;(prn "up")
;                            (let [{:keys [dragging-id]} @state
;                                  [x y] (get-normalized-coords e)]
;                              (swap! state dissoc :dragging-id)
;                              (.removeEventListener js/document "mousemove" handle-mouse-move)
;                              (.removeEventListener js/document "mouseup" mouse-up)
;                              (when on-drag-end (on-drag-end dragging-id x y))))

;         handle-mouse-down  (fn [e]
;                              ;;(prn "down")
;                              (.removeAllRanges (.getSelection js/window))
;                              (let [bounds (-> e .-target .getBoundingClientRect)
;                                    dx (- (.-clientX e) (/ (+ (.-left bounds) (.-right bounds)) 2))
;                                    dy (- (.-clientY e) (/ (+ (.-top bounds) (.-bottom bounds)) 2))
;                                    dragging-id (-> e .-currentTarget (.getAttribute "data-id"))
;                                    [x y] (get-normalized-coords e)]
;                                (swap! state assoc :dragging-id dragging-id :dx dx :dy dy)
;                                (.addEventListener js/document "mousemove" handle-mouse-move)
;                                (.addEventListener js/document "mouseup" handle-mouse-up)
;                                (when on-drag-start (on-drag-start dragging-id x y))))

;         handle-window-resize (fn [e] (swap! state assoc :window-resized? true))]

;     (r/create-class
;      {:component-did-mount (fn [this]
;                             ;  (prn "component-did-mount")
;                              (.addEventListener js/window "resize" handle-window-resize)
;                              (swap! state assoc :area-node (r/dom-node this)))

;       :component-will-unmount (fn [this]
;                                 (.removeEventListener js/window "resize" handle-window-resize))

;       :component-did-update (fn [prev-props prev-state]
;                               ; (prn "component-did-update")
;                               (when-let [root (:area-node @state)]
;                                 (let [bounds (.getBoundingClientRect root)]
;                                   (swap! state assoc
;                                          :window-resized? false
;                                          :area-width (- (.-right bounds)  (.-left bounds))
;                                          :area-height (- (.-bottom bounds) (.-top bounds))))))

;       :reagent-render (fn [{:keys [render-function]}]
;                         ; (prn "reagent-render")
;                         (let [{:keys [area-width area-height]} @state]
;                           [:div.svg-container {:style {:width "100%" :height "100%"
;                                                       ;  :background-color :yellow
;                                                        :overflow :hidden}}
;                            [:svg {; :width area-width :height area-height
;                                   :width "100%" :height "100%"
;                                   :style {:background-color :aliceblue}}
;                                         ;;{:viewBox "0 0 1 1"}
;                             (into [:<>] (render-function {:norm-x #(norm % area-width)
;                                                           :norm-y #(norm % area-height)
;                                                           :on-mouse-down handle-mouse-down})) ;
;                             ]]))})))


; (defn points-view-- [main-state]
;   (let [{:keys [coords route]} @main-state]
;     [points-view-core--
;      {:render-function (fn [{:keys [norm-x norm-y on-mouse-down]}]
;                          [(when-not (empty? route)
;                             [:polyline {:points (->> route
;                                                      (map (fn [id]
;                                                             (let [[x y] (coords id)]
;                                                               (str (norm-x x) "," (norm-y y)))))
;                                                      (str/join " "))
;                                         :fill :none :stroke :black :stroke-width 3}])
;                           (for [[id [x y]] coords]
;                             ^{:key #_id (gensym)}
;                             [:circle (merge {:cx (norm-x x) :cy (norm-y y)
;                                              :r 8 :stroke "#444" :stroke-width 3 :fill :yellow
;                                              :onMouseDown on-mouse-down
;                                              :data-id id}
;                                             (case id
;                                               "base"  {:r 12 :stroke-width 0 :fill :brown}
;                                               "start" {:r 10 :stroke-width 0 :fill :green}
;                                               nil))])])
;       :on-drag-start (fn [data-id x y] (prn "on-drag-start" data-id x y))
;       :on-drag (fn [data-id x y] (swap! main-state assoc-in [:coords data-id] [x y]))
;       :on-drag-end (fn [data-id x y] (prn "on-drag-end__" data-id x y) (run main-state))} ;
;      ]))


; (let [v [0 1 2 3]]
;   (into
;    (for [i v] #{i})
;    (for [i v j v :when (< i j)] #{i j})))

; Человек A может пересечь мост за одну минуту, B за две минуты, C за пять минут и D за восемь минут.

; (def ts [1 2 5 8])
; (def u (-> ts count range set))

; (defn steps [v]
;   (let [{:keys [l t]} (last v)
;         l? (-> v count odd?)
;         from (if l? l (set/difference u l))
;         es (->> v (map :l) set)]
;     (for [i from
;           j from
;           :let [l ((if l? disj conj) l i j)
;                 t (+ t (max (ts i) (ts j)))]
;           :when (and (<= i j) (<= t 15) (not (es l)))]
;       (conj v {:l l :t t}))))

; (defn f [vs] (mapcat #(if (-> % last :l empty?) [%] (f (steps %))) vs))

; (comment

; (f [ [{:l u :t 0}] ])
; (+ 1 2 3)

; )
; (defn improve []
;   (let [start-time (system-time)

;         {:keys [route settings]} @state
;         ; t1 (f-2-opt-1 route)
;         t1-route-distance (common/route-distance route)


;         lo (local-optimisation-heuristics
;             (get-in @state [:settings :local-optimisation-heuristic]))

;         data (->> @state
;                   nearest-neighbor/make-route
;                   ; multi-fragment-start-tour
;                   (lo settings))
;         t2-route-distance (common/route-distance data)
;         delta (/ (- t1-route-distance t2-route-distance) t1-route-distance)]
;     ; (when (pos? delta)
;     (prn "improve"
;          (-> delta (* 100) int (str " %"))
;          (-> t2-route-distance (* 100) int))
;     (reset-route data start-time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; (defn continue? []
;   (let [{:keys [counter]} @state]
;     (> counter 0)))


; (defn on-tik [state]
;   (let [{:keys [curr-tour route best-route-distance t cooling-rate number-of-iterations]} @state
;         new-tour (mutation route) ; (mutation curr-tour)
;         new-route-distance (route-distance new-tour)
;         p (if (< new-route-distance best-route-distance)
;             1
;             (Math/exp (/ (- best-route-distance new-route-distance) best-route-distance 0.0001 t)) ;

;             ; (/ (- new-route-distance best-route-distance) best-route-distance 0.01 t)
;             )]
;     ; (print (str number-of-iterations " " (.toFixed t 2) " " (int (* 100 p)) " " best-route-distance))
;     (print (str (int t) " " (int (* 100 p)) " " (.toFixed (or best-route-distance 0) 2)))
;     (when (< (Math/random) p)
;       (swap! state assoc
;           ;  :curr-tour new-tour
;              :route new-tour
;              :best-route-distance new-route-distance))
;     (swap! state assoc :t (* t 0.99) :number-of-iterations (inc number-of-iterations))
;     (and
;      (:simulation-on @state)
;      (> t 1))
;     ; (continue?)
;     ))

; (defn periodic [f v]
;   (-> (js/Promise. (fn [resolve] (js/setTimeout #(resolve (f v)) 100)))
;       (.then #(when % (periodic f v)))
;       (.catch prn)))

; (defn stop-go [state]
;   (swap! state update :simulation-on not)
;   (when (:simulation-on @state) (periodic on-tik state)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2-opt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; (defn tst []
;   (let [loop-count 100]
;     (loop [i loop-count
;            mi nil
;            ma nil
;            s 0]
;       (if (pos? i)
;         (let [start-data (common/random-data 20)
;               ; _ (prn "-------------------------")
;               t1-route-distance (route-distance (f-2-opt (greedy-tour start-data)))
;               t2-route-distance (route-distance (f-2-opt (multi-fragment-start-tour
;                                                         #_insertions-2-start-tour
;                                                         start-data)))
;               c (-> (- t1-route-distance t2-route-distance) (/ t1-route-distance) (* 100) int)]
;           ; (prn t1-route-distance t2-route-distance)
;           (recur (dec i)
;                  (if mi (min mi c) c)
;                  (if ma (max ma c) c)
;                  (+ s c)))
;         (prn [mi ma (/ s loop-count)])))))

; (defn improve-- []
;   (let [_ (reset! stat-3-opt nil)

;         {:keys [route]} @state
;         ; t1 (f-2-opt-1 route)
;         t1-route-distance (route-distance route)

;         opt-2-s (time (->> (greedy-tours (assoc @state :random? false #_true))
;                            (mapv f-2-opt #_-fix-r)))

;         data (safe-min-key route-distance opt-2-s)
;         _ (prn (->> opt-2-s
;                     (map #(-> % route-distance (* 100) int))
;                     frequencies
;                     (sort-by first)))

;         ; data (time (->> (greedy-tours (assoc @state :random? false #_true))
;         ;                 (map f-2-opt)
;         ;                 (safe-min-key route-distance)))
;         ; data (time (->>
;         ;             ; @state
;         ;             ; greedy-tour
;         ;             route
;         ;             ; f-3-opt
;         ;             f-2-opt))
;         t2-route-distance (route-distance data)
;         delta (/ (- t1-route-distance t2-route-distance) t1-route-distance)]
;     (when (pos? delta)
;       (prn "improve"
;            (-> delta (* 100) int (str " %"))
;            (-> t2-route-distance (* 100) int))

;       (swap! state assoc :route data))
;     ; (prn @stat-3-opt)
;     ))

; (defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

; (defn tr [points f]
;   (loop [ps points
;          i 5]
;     (f ps)
;     (fib 38)
;     (prn i)
;     (if (<= i 0) points
;         (recur (shuffle points) (dec i)))))