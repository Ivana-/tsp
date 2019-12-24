(ns tsp.view
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [widgets.atom-based :as widgets]
            [shared.utils :as utils]
            [shared.common :as common]
            [route-construction.random :as random]
            [route-construction.nearest-neighbor :as nearest-neighbor]
            [route-construction.multi-fragment :as multi-fragment]
            [local-optimization.local-2-opt :as local-2-opt]
            [charts.svg :as charts-svg]))

(def default-settings {:points-amount 20
                       :subroutes-amount 2
                       :max-points-in-subroute 12
                       :random-start-tours-count 1
                       :route-construction-heuristic "nearest-neighbor"
                       :local-optimisation-heuristic "2-opt"})

(defn random-points [state]
  (let [settings (:settings @state)]
    (reset! state {:settings settings
                   :coords (common/random-data (:points-amount settings))})))

(def route-construction-heuristic {"random" random/make-routes
                                   "nearest-neighbor" nearest-neighbor/make-routes
                                   "multi-fragment" multi-fragment/make-routes})

(def local-optimisation-heuristics {"2-opt" local-2-opt/best-route})

(defn run [state]
  (let [start-time (system-time)
        {:keys [settings coords]} @state
        make-routes (route-construction-heuristic
                     (:route-construction-heuristic settings))
        local-optimisation (or (local-optimisation-heuristics
                                (:local-optimisation-heuristic settings))
                               (fn [s x] x))
        local-optimized (->> (merge @state settings)
                             (make-routes (:random-start-tours-count settings))
                             (map #(local-optimisation @state %)))
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

(defn chart [state]
  (let [{:keys [coords route]} @state
        ; on-drag-start (fn [data-id x y] (prn "on-drag-start" data-id x y))
        on-drag (fn [data-id x y] (swap! state assoc-in [:coords data-id] [x y]))
        on-drag-end (fn [data-id x y] #_(prn "on-drag-end__" data-id x y) (run state))]
    [charts-svg/chart
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

          [:h2 {:style {:align-self :center}} "Algorithm"]
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

         [chart state]]))))
