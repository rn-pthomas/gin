(ns gin.server.audio
  (:require [clojure.java.io       :as io]
            [clojure.core.async    :as a]
            [gin.server.framework  :as f]
            [gin.server.processing :as process])
  (:import [javax.sound.sampled AudioFormat AudioFormat$Encoding AudioInputStream AudioFileFormat AudioFileFormat$Type AudioSystem DataLine DataLine$Info Mixer SourceDataLine TargetDataLine]
           [java.io ByteArrayOutputStream]))

(defn create-audio-format
  [& [{:keys [format sample-rate bits-per-sample channels frame-size frame-rate little-endian]
       :or {format          AudioFormat$Encoding/PCM_SIGNED
            sample-rate     48000
            bits-per-sample 16
            channels        2
            frame-size      4
            frame-rate      sample-rate
            little-endian   false}}]]
  (AudioFormat. format sample-rate bits-per-sample channels frame-size frame-rate little-endian))

(defn ->info
  [^AudioFormat audio-format ^Class clazz]
  (DataLine$Info. clazz audio-format))

(defn get-mixers
  []
  (let [mixer-objs (AudioSystem/getMixerInfo)]
    (map (fn [mixer-obj]
           {:name        (.getName mixer-obj)
            :description (.getDescription mixer-obj)
            :version     (.getVersion mixer-obj)
            :vendor      (.getVendor mixer-obj)
            :obj         mixer-obj})
         mixer-objs)))

(defn get-mobile-pre-device
  []
  (->> (get-mixers)
       (filter #(= (:name %) "MobilePre"))
       first))

(defn input-type->target-line
  [^clojure.lang.Keyword input-type audio-format]
  (let [target-info (->info audio-format TargetDataLine)]
    (condp = input-type
      :mic       (AudioSystem/getLine target-info)
      :interface (AudioSystem/getTargetDataLine audio-format (:obj (get-mobile-pre-device)))
      ;; else
      (throw (Exception. (format "Couldn't look up TargetDataLine for input type %s" (name input-type)))))))

(defn input-type->source-line
  [^clojure.lang.Keyword input-type audio-format]
  (let [target-info (->info audio-format SourceDataLine)]
    (condp = input-type
      :mic       (AudioSystem/getLine target-info)
      :interface (AudioSystem/getSourceDataLine audio-format (:obj (get-mobile-pre-device)))
      ;; else
      (throw (Exception. (format "Couldn't look up SourceDataLine for input type %s" (name input-type)))))))

(defn open-and-start
  [line]
  (.open line)
  (.start line)
  line)

(defn cleanup
  [{:keys [threads lines]}]
  (doseq [thread threads]
    (.stop thread))
  (doseq [line lines]
    (.stop line)
    (.close line)))

(defn recording-thread-handler
  [{:keys [sample-chan data-bytes target-line source-line byte-array-size sample-store]}]
  (println "recording-thread-handler started")
  (let [audio-stream (AudioInputStream. target-line)
        frame-size   (.getFrameLength audio-stream)]
    (loop [num-bytes-read (.read audio-stream data-bytes)]
      (let [bytes-data (vec data-bytes)]
        (do
          (a/>!! sample-chan {:bytes      {:size num-bytes-read
                                           :data bytes-data}
                              :frame-size frame-size})
          (recur (.read audio-stream data-bytes)))))))

(defn samples->clip
  [messages]
  (reduce (fn [acc sample]
            (let [bytes-size (get-in sample [:bytes :size])
                  bytes-data (get-in sample [:bytes :data])]
              (-> acc
                  (update-in [:bytes :size] (partial + bytes-size))
                  (update-in [:bytes :data] #(concat % bytes-data)))))
          {:bytes {:size 0
                   :data []}}
          messages))

(defn concat-bytes
  [byte-chunks]
  (reduce (fn [acc chunk]
            (let [bytes-size (get-in chunk [:bytes :size])
                  bytes-data (get-in chunk [:bytes :data])]
              (-> acc
                  (update-in [:bytes :size] (partial + bytes-size))
                  (update-in [:bytes :data] #(concat % bytes-data)))))
          {:bytes {:size 0
                   :data []}}
          byte-chunks))

(defn sample-thread-handler
  "Reads from sample-chan, buffers samples, applies processing, stores in clip store."
  [{:keys [sample-chan data-bytes target-line source-line byte-array-size sample-store buffer-size clip-chan clip-store]
    :or   {buffer-size 1}}]
  (println "sample-thread-handler started")
  (while true
    (f/buffer-n-messages
     sample-chan
     buffer-size
     (fn [messages]
       (let [clip (samples->clip messages)]
         (swap! clip-store conj clip)
         (a/>!! clip-chan :ok))))))

(defn clip-thread-handler
  [{:keys [sample-chan data-bytes target-line source-line byte-array-size sample-store sample-buffer-size clip-chan clip-store playback-chan done-chan composition]
    :or   {sample-buffer-size 1}}]
  (if composition
    (do
      (doseq [[idx clip-idx] (map-indexed (fn [idx item] [idx item]) composition)]
        (let [got-a-new-clip (a/<!! clip-chan) ;; block until clip is available
              ]
          (when-let [clip (get @clip-store clip-idx)]
            (let [process-fn (cond
                               (= (/ idx 4) 0)
                               process/halve-frame-rate

                               (= (/ idx 2) 0)
                               process/quarter-frame-rate

                               :else
                               process/reverse)]
              (println (format "clip available. idx = %s, clip-idx = %s" idx clip-idx))
              ;;(a/>!! playback-chan (process-fn clip))
              (a/>!! playback-chan clip))))))
    (loop [step 0]
      (let [got-a-new-clip  (a/<!! clip-chan)
            last-four-clips (take-last 1 @clip-store)
            process-fn      (case step
                              0 process/halve-frame-rate
                              1 process/reverse
                              2 process/quarter-frame-rate)
            clips           (concat-bytes last-four-clips)
            clips           (process-fn clips)
            new-step        (if (>= step 2)
                              0
                              (inc step))]
        (do
          (a/>!! playback-chan clips)
          (recur new-step))))))

(defn playback-thread-handler
  [{:keys [sample-chan data-bytes target-line source-line byte-array-size sample-store sample-buffer-size clip-chan clip-store playback-chan]
    :or   {sample-buffer-size 1}}]
  (println "playback-thread-handler started")
  (while true
    (let [{:keys [bytes]}     (a/<!! playback-chan)
          {:keys [data size]} bytes]
      (when (and data size)
        (.write source-line (byte-array size data) 0 size)))))

(defn seconds->ms
  [seconds]
  (* 1000 seconds))

(defn stream!
  [{:keys [ms init-ms input-type buffer-size composition] :as params
    :or   {init-ms 0}}]
  (let [audio-format        (create-audio-format {:sample-rate 44100 :channels 2})
        target-line         (input-type->target-line input-type audio-format)
        source-line         (input-type->source-line input-type audio-format)
        _                   (open-and-start target-line)
        _                   (open-and-start source-line)
        byte-array-size     (/ (.getBufferSize target-line) 5)
        data-bytes          (byte-array byte-array-size)
        audio-chan          (a/chan 2000)
        sample-chan         (a/chan 2000)
        clip-chan           (a/chan 2000)
        playback-chan       (a/chan 2000)
        done-chan           (a/chan 1)
        sample-store        (atom [])
        clip-store          (atom [])
        thread-params       {:target-line     target-line
                             :source-line     source-line
                             :data-bytes      data-bytes
                             :byte-array-size byte-array-size
                             :clip-chan       clip-chan
                             :sample-chan     sample-chan
                             :playback-chan   playback-chan
                             :done-chan       done-chan
                             :clip-store      clip-store
                             :sample-store    sample-store
                             :buffer-size     buffer-size
                             :composition     composition}
        recording-thread    (Thread. #(recording-thread-handler thread-params))
        sample->clip-thread (Thread. #(sample-thread-handler    thread-params))
        clip-thread         (Thread. #(clip-thread-handler      thread-params))
        playback-thread     (Thread. #(playback-thread-handler  thread-params))]
    (.start recording-thread)
    (.start sample->clip-thread)
    (.start clip-thread)
    (Thread/sleep init-ms) ;; capture initial audio data before starting playback
    
    (.start playback-thread)
    (Thread/sleep (+ init-ms ms))
    (cleanup {:threads [recording-thread sample->clip-thread clip-thread playback-thread]
              :lines   [target-line source-line]})
    (println (format "Done. Captured %s clips" (count @clip-store)))
    (println (format "Done. params = %s" params))
    
    :done))

(defn continuously-stream!
  [{:keys [input-type]}])
