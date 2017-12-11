(ns ^{:doc "Functions for applying processing to byte arrays of audio data"}
    gin.server.processing)

(defn mk-processor
  [handler-fn]
  (let [bytes-per-sample 2]
    (fn [{:keys [bytes]}]
      (let [{:keys [data size]} bytes
            processed-data      (->> data
                                     (partition bytes-per-sample)
                                     handler-fn
                                     flatten
                                     vec)]
        {:bytes {:data (byte-array size processed-data)
                 :size size}}))))

(def reverse
  (mk-processor clojure.core/reverse))

(def halve-frame-rate
  (let [chop-factor 2]
    (mk-processor (fn [data]
                    (->> data
                         (partition 2)
                         (take-nth chop-factor)
                         (repeat chop-factor))))))

(def quarter-frame-rate
  (let [chop-factor 4]
    (mk-processor (fn [data]
                    (->> data
                         (partition 2)
                         (take-nth chop-factor)
                         (repeat chop-factor))))))

(comment
  (->> (halve-frame-rate {:bytes {:data (range 24)
                                  :size 24}})
       :data
       vec)
)
