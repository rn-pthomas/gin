(ns gin.app.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(defonce app-state (r/atom {:socket {:connected false}
                            :state  {:messages []
                                     :running  false}}))

(defn update-state!
  [key-path update-fn]
  (swap! app-state update-in (concat [:state] key-path) update-fn))

(defn get-state
  [key-path]
  (get-in @app-state (concat [:state] key-path)))

(defn input-evt->text
  [evt]
  (-> evt .-target .-value))

(defn handle-ws-message
  [msg]
  (update-state! [:messages] #(conj % msg)))

(defn send-ws-message
  [msg-type & [msg-data]]
  (let [ws   (get-in @app-state [:socket :val])
        data (.stringify (.-JSON js/window) (clj->js {:type msg-type
                                                      :data msg-data}))]
    (.send ws data)))

(def websocket-handler-dispatch
  {"onopen"    #(println "open")
   "onclose"   #(println "close")
   "onerror"   (fn [err] (println "error:" err))
   "onmessage" (fn [msg]
                 (let [parsed-data (js->clj (.parse (.-JSON js/window) (.-data msg)))]
                   (handle-ws-message parsed-data)))})

(defn init-websocket
  [url]
  (when-not (true? (get-in @app-state [:socked :connected]))
    (let [ws (js/WebSocket. url)]
      (doseq [[action handler] websocket-handler-dispatch]
        (aset ws action handler))
      ws)))

(def dimension 4)
(def style-factor 60)

(defn make-style
  [x y]
  {:top  (* y style-factor)
   :left (* x style-factor)})

(defn cell
  [x y]
  (let [highlighted (r/atom false)]
    (fn [x y]
      (println "rendering... x =" x " y =" y)
      (let [css-class   (if @highlighted
                          :div.cell.highlighted
                          :div.cell)]
        [css-class {:style    (make-style x y)
                    :key      (str "x." x ".y." y)
                    :data-x   x
                    :data-y   y
                    :on-click (fn [_]
                                (swap! highlighted not))}]))))

(defn main-component
  []
  [:div
   [:div#grid
    (doall
     (for [y (range dimension)
           x (range dimension)]
       [cell x y]))]
   [:button#start-playback
    {:style {:left 0
             :top (* dimension style-factor)}
     :on-click (fn [_]
                 (send-ws-message :toggle-playback)
                 (update-state! [:running] not))}
    (if (true? (get-state [:running]))
      "Stop"
      "Start")]])

(defn main
  []
  (let [ws (init-websocket "ws://localhost:9091")]
    (swap! app-state assoc-in [:socket :val] ws)
    (r/render [main-component] (.getElementById js/document "app"))))

(defn on-js-reload
  []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (main))

(main)
