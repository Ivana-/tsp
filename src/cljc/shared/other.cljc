(ns shared.other
  (:require [shared.utils :as utils]))


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

