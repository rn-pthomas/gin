(ns gin.server.dsl
  (:require [gin.server.audio :as audio]))

(defn sample
  [^clojure.lang.Keyword sample-id sample-params]
  {:type :sample :id sample-id :params sample-params})

(defn clip
  "A clip is a collection of samples"
  [^clojure.lang.Keyword clip-id sample-ids]
  {:type :clip :id clip-id :sample-ids sample-ids})

(defn validate-samples!
  [samples sample-ids]
  (let [sample-ids-from-sample-maps-set (->> samples (map :id) set)]
    (every? (fn [sample-id]
              (def sample-id sample-id)
              (not (nil? (get sample-ids-from-sample-maps-set sample-id))))
            sample-ids)))

(defn composition
  "Defines a set of clips that can be used to use process streaming audio, and how those clips will be played"
  [& things]
  (let [filter-by-type  (fn [t]
                          (filter #(= (:type %) t) things))
        clips           (filter-by-type :clip)
        samples         (filter-by-type :sample)
        play-statements (filter-by-type :play)
        sample-ids      (distinct (mapcat :sample-ids clips))]
    (validate-samples! samples sample-ids)
    {:samples samples
     :clips   clips
     :play    play-statements}))

(defn play
  [& clip-ids]
  [clip-ids])

(comment
  (composition
   (sample 0 {:length 2})
   (sample 1 {:length 1})
   (sample 2 {:length 2})
   (clip :a [0 1 2])
   (clip :b [2 1 0])
   (play :a :b :a :b))
)

