(ns ^{:doc "Generic framework for asynchronous chunking and processing of data"}
    gin.server.framework
  (:require [clojure.core.async :as a]
            [schema.core        :as s]))

(defn buffer-n-messages
  "Use blocking take to take n messages off of channel c, and once done, applies function f to that vec of messages."
  [ch n f]
  (loop [acc     []
         counter 0]
    (if (>= counter n)
      (f acc)
      (let [new-message (a/<!! ch)]
        (recur (conj acc new-message) (inc counter))))))

(defn buffer-then-put
  "Accumulates n messages from in-chan with blocking-take, and once complete, puts that collection to out-chan"
  [in-chan n out-chan & [xform-fn]]
  (buffer-n-messages in-chan n (fn [messages]
                                 (a/>!! out-chan (if xform-fn
                                                   (xform-fn messages)
                                                   messages)))))

(s/defn mk-thread
  "Returns the thread itself, a start function, and a stop function."
  [inner-handler-fn params]
  (let [thread-obj (Thread. #(inner-handler-fn params))]
    {:thread   thread-obj
     :start-fn (fn [] (.start thread-obj))
     :stop-fn  (fn [] (.stop thread-obj))}))

(defn stop-threads
  [& threads]
  (doseq [{:keys [stop-fn]} threads]
    (stop-fn)))
