(ns tsp.core
  (:require [reagent.core :as reagent]
            [tsp.view :as view]))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(def debug? ^boolean goog.DEBUG)

(defn dev-setup []
  (when debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (reagent/render [view/main-panel] (.getElementById js/document "app")))

(defn ^:export init []
  (dev-setup)
  (mount-root))
