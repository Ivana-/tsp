(ns tsp.widgets)

(defn input-range [{:keys [state path] :as params}]
  [:input (merge {:type "range"
                  :min 1
                  :max 100
                  :step 1
                  :class :input
                  :value (get-in @state path)
                  :on-change #(swap! state assoc-in path (js/parseFloat (.. % -target -value)))}
                 (dissoc params :state :path))])

(defn input-checkbox [{:keys [state path] :as params}]
  [:input (merge {:type "checkbox"
                  :class :input
                  :checked (= "true" (str (get-in @state path)))
                  :on-change #(swap! state assoc-in path (.. % -target -checked))}
                 (dissoc params :state :path))])

(defn input-integer [{:keys [state path] :as params}]
  [:input (merge {:type "number"
                  :class :input
                  ;;:step "1"
                  :value (str (get-in @state path))
                  :on-change #(swap! state assoc-in path (js/parseFloat (.. % -target -value)))}
                 (dissoc params :state :path))])

(defn input-textarea [{:keys [state path] :as params}]
  [:textarea (merge {:class :input
                     :value (str (get-in @state path))
                     :on-change #(swap! state assoc-in path (.. % -target -value))}
                    (dissoc params :state :path))])

(defn input-select [{:keys [state path items] :as params}]
  (into
   [:select (merge {:class :input
                    :value (str (get-in @state path))
                    :on-change #(swap! state assoc-in path (.. % -target -value))}
                   (dissoc params :state :path :items))]
   (mapv (fn [x] [:option {:value x} x]) items)))

(defn input-radio [{:keys [state path items label-left? child-style] :as params}]
  (let [name (str (gensym))]
    (into
     [:div (merge {:class :input}
                  (dissoc params :state :path :items :label-left? :child-style))]
     (mapv (fn [i {:keys [value label] :as item}]
             (let [value (or value item)
                   id (str name i)
                   label-element [:label {:for id
                                          :style {:vertical-align :top}}
                                  (or label value item)]]
               [:div {:style child-style}
                (when label-left? label-element)
                [:input (merge {:type "radio"
                                :id id
                                :name name
                                :value value
                                :checked (= value (str (get-in @state path)))
                                :on-change #(swap! state assoc-in path (.. % -target -value))})]
                (when-not label-left? label-element)]))
           (range) items))))

; [settings-panel-row
;  (widgets/input-radio {:state state
;                        :path [:settings :once-or-multiple]
;                        :items ["once" "multiple"]
;                        :label-left? true
;                        :style {:display :flex}
;                        :child-style {:margin-bottom "10px"
;                                      :margin-left "1em"}})]
