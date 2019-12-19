(ns tsp.view
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [widgets.atom-based :as widgets]
            [shared.utils :as utils]
            [shared.common :as common]
            
            [route-construction.random :as random]
            [route-construction.nearest-neighbor :as nearest-neighbor]
            [route-construction.multi-fragment :as multi-fragment]

            [local-optimization.local-2-opt :as local-2-opt]

            [charts.svg :as charts-svg]
            
            ))

(rf/reg-event-db
 :initialize-db
 (fn [_ _] {:config 333 ; {:backend-url config/backend-url}
 }))



(rf/reg-event-db
 :set-points
 (fn [db [_ data]]
 (prn :set-points)
   (assoc db :points data)))

(rf/reg-sub
 :points
 (fn [db _] (:points db)))



(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))


; (defn tr [points f]
;   (loop [ps points
;          i 5]
;     (f ps)
;     (fib 38)
;     (prn i)
;     (if (<= i 0) points
;         (recur (shuffle points) (dec i)))))


(def default-settings {:points-amount 20
                       :subroutes-amount 2
                       :max-points-in-subroute 12
                       :random-start-tours-count 1
                       :route-construction-heuristic "multi-fragment" ; "nearest-neighbor" ; "multi-fragment"
                       :local-optimisation-heuristic "" ; "2-opt"
                       :once-or-multiple "multiple"})

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


(defn random-points [state]
  (let [settings (:settings @state)]
    (reset! state {:settings settings
                   :coords (common/random-data (:points-amount settings))})))

(def route-construction-heuristic {"random" random/make-routes
                                   "nearest-neighbor" nearest-neighbor/make-routes
                                   "multi-fragment" multi-fragment/make-routes})

(def local-optimisation-heuristics {"2-opt" local-2-opt/best-route})

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


(defn run [state]
  (let [start-time (system-time)
        {:keys [settings coords]} @state
        make-routes (route-construction-heuristic
                     (:route-construction-heuristic settings))
        lo (local-optimisation-heuristics
            (:local-optimisation-heuristic settings))
        lo (or lo (fn [s x] x))
        local-optimized (->> (merge @state settings)
                             (make-routes (:random-start-tours-count settings))
                             (map #(lo @state %)))

        route (utils/safe-min-key (partial common/route-distance* coords) local-optimized)]
    (prn (->> local-optimized
              (map #(->> % (common/route-distance* coords) (* 100) Math/round))
              frequencies
              (sort-by first)))
    (swap! state merge {:route route
                        :statistics {:curr-elapsed-time (Math/round (- (system-time) start-time))
                                     :prev-elapsed-time (get-in @state [:statistics :curr-elapsed-time])
                                     :curr-distance (->> route (common/route-distance* coords) (* 100) Math/round)
                                     :prev-distance (get-in @state [:statistics :curr-distance])}})))



(defn points-view [state]
  (let [{:keys [coords route]} @state
        ; on-drag-start (fn [data-id x y] (prn "on-drag-start" data-id x y))
        on-drag (fn [data-id x y] (swap! state assoc-in [:coords data-id] [x y]))
        on-drag-end (fn [data-id x y] #_(prn "on-drag-end__" data-id x y) (run state))]
    [charts-svg/points-view-core
     {:objects (into [(when-not (empty? route)
                        [:polyline {:points (map #(let [[x y] (coords %)] {:x x :y y}) route)
                                    :fill :none :stroke :black :stroke-width 3}])]
                     (for [[id [x y]] coords]
                       [:circle (merge {:cx x :cy y
                                        :r 8 :stroke "#444" :stroke-width 3 :fill :yellow
                                        ; :on-drag-start on-drag-start
                                        :on-drag on-drag
                                        :on-drag-end on-drag-end
                                        :data-id id}
                                       (case id
                                         "base"  {:r 12 :stroke-width 0 :fill :brown}
                                         "start" {:r 10 :stroke-width 0 :fill :green}
                                         nil))]))
      :style {:background-color :aliceblue}}]))


(defn settings-panel-row [& ws]
  (->> (reduce (fn [acc x]
                 (conj acc
                       (when-not (empty? acc) [:div {:style {:width "1em"}}])
                       (if (string? x) [:label x] x)))
               [] ws)
       (into [:div {:style {:display :flex
                            :align-items :flex-end
                            :white-space :nowrap
                            :padding "0 20px 10px 20px"}}])))

(defn val-percent [curr-value prev-value units]
  [:div {:style {:margin "0 20px"}}
   (when curr-value
     [:span {:style {:font-size "5em"}} (str curr-value)])
   (when (and curr-value units)
     [:span {:style {:font-size "3em"}} (str " " units)])
   (when (and (number? curr-value) (number? prev-value) (not (zero? prev-value)))
     (let [p (-> (- prev-value curr-value) (/ prev-value) double (* 100))]
       [:div {:style {:font-size "3em"
                      ; :margin "20px"
                      :color (cond (pos? p) :green
                                   (neg? p) :red
                                   :else :auto)}}
        (cond (pos? p) (str "-" (Math/round p) "%")
              (neg? p) (str "+" (Math/round (- p)) "%")
              :else (str (Math/round p) "%"))]))])

(defn main-panel []
  (let [state (r/atom {:settings default-settings})]
    (fn []
      (let [{:keys [coords statistics]} @state
            {:keys [curr-elapsed-time prev-elapsed-time
                    curr-distance prev-distance]} statistics
            flex-column-end {:display :flex
                             :flex-direction :column}]
        [:div {:style {:display :flex
                       :height "100%"}}
         [:div {:style flex-column-end}

          [:h2 {:style {:align-self :center}} "Initial data"]
          [settings-panel-row
           [:button {:on-click #(random-points state)} "Generate"]
           (widgets/input-integer {:state state :path [:settings :points-amount]
                                   :style {:width "3em"}})
           "random points (+ start & base)"]

          [:h2 {:style {:align-self :center}} "Algorithms"]
          [:fieldset {:style {:border :none
                              :margin 0
                              :padding 0}
                      :disabled (not coords)}
           [:div {:style flex-column-end}
            [settings-panel-row
             (widgets/input-integer {:state state :path [:settings :subroutes-amount]
                                     :style {:width "3em"}})
             "subroutes of max"
             (widgets/input-integer {:state state :path [:settings :max-points-in-subroute]
                                     :style {:width "3em"}})
             "points"]
            [settings-panel-row
             "route construction heuristic"
             (widgets/input-select {:state state
                                    :path [:settings :route-construction-heuristic]
                                    :items (keys route-construction-heuristic)})]
            [settings-panel-row
             "local optimisation heuristic"
             (widgets/input-select {:state state
                                    :path [:settings :local-optimisation-heuristic]
                                    :items (into [""] (keys local-optimisation-heuristics))})]
            [settings-panel-row
             [:button {:on-click #(run state)} "Show the best"]
             "of"
             (widgets/input-integer {:state state :path [:settings :random-start-tours-count]
                                     :style {:width "3em"}})
             "radomized start heuristics"]]]

          (when statistics
            [:<>
             [:h2 {:style {:align-self :center}} "Statistics"]
             (val-percent curr-distance prev-distance "km")
             (val-percent curr-elapsed-time #_prev-elapsed-time nil "ms")])]

         [points-view state]]))))
