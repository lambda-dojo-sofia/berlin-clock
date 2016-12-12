(ns berlin-clock.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def app-clock (atom (js/Date.)))

(defn bulb [state]
  [:div.bulb {:class state}])

(defn bulbs-row [bulbs state]
  [:div.clock-row
   (doall
    (map-indexed #(bulb (if (and (= state "special")
                                 (zero? (mod (inc %1) 3)))
                          "red"
                          (if (not= state "white")
                            "yellow"
                            "white"))) (range bulbs)))])

(defn seconds-component [seconds]
  [:div.seconds
   [bulbs-row 1 (if (even? seconds)
                  "yellow"
                  "white")]])

(defn bulbs-line-component [toggled size]
  [:div.bulb-line
   [bulbs-row toggled (if (= size 11)
                        "special"
                        "yellow")]
   [bulbs-row (- size toggled) "white"]])

(defn set-clock! []
  (prn "Setting the time baby!")
  (reset! app-clock (js/Date.)))

(def clock
  (with-meta (fn []
               (let [hours        (.getHours @app-clock)
                     minutes      (.getMinutes @app-clock)
                     seconds      (.getSeconds @app-clock)]
                 [:div.clock
                  [seconds-component seconds]
                  [bulbs-line-component (quot hours 5) 4]
                  [bulbs-line-component (mod hours 5) 4]
                  [bulbs-line-component (quot minutes 5) 11]
                  [bulbs-line-component (mod minutes 5) 4]]))
    {:component-did-mount #(do
                             (prn "We are here?")
                              (js/setInterval set-clock! 1000))}))

(reagent/render-component [clock]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
