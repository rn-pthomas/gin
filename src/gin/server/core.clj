(ns gin.server.core
  (:require [org.httpkit.server         :as server]
            [cheshire.core              :as json]
            [clojure.tools.nrepl.server :as nrepl]))

(defn start-nrepl-server
  []
  (let [port 7888]
    (nrepl/start-server :port port)
    (println (format "Nrepl running on port %s" port))))

(def msg-handler-dispatch
  {"start-playback" (fn [msg-data]
                      (println (format "implement start-playback: %s" msg-data)))})

(defn on-receive-handler
  [data channel]
  (let [{msg-type :type
         msg-data :data} (json/parse-string data keyword)
        handler          (get msg-handler-dispatch msg-type)]
    (if handler
      (let [resp-data (handler msg-data)]
        (server/send! channel (json/generate-string resp-data)))
      (println (format "Error: unrecognized msg-type: %s" msg-type)))))

(defn websocket-handler
  [request]
  (server/with-channel request channel
    (server/on-close channel (fn [status]
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data]
                                 (on-receive-handler data channel)))))

(defn main
  []
  (server/run-server #'websocket-handler {:port 9091})
  (start-nrepl-server))

(comment
  (main)
  )
