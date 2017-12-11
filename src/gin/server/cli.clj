(ns gin.server.cli
  (:require [clojure.tools.cli :as c]
            [gin.server.audio  :as audio]))

(def cli-options
  [["-m" "--ms MILLISECONDS" "Millseconds to run program"
    :default 5000
    :parse-fn #(Integer/parseInt %)]
   ["-i" "--init-ms INIT MILLSECONDS" "Milliseconds to buffer audio before streaming"
    :default 1000
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--input-type INPUT TYPE" "Type of audio input (mic, interface)"
    :default "mic"
    :parse-fn keyword]
   ["-o" "--output-file OUTPUT FILE" "File to save to"]])

(defn stream!
  [& args]
  (let [{:keys [options]}               (c/parse-opts args cli-options)
        {:keys [ms init-ms input-type]} options]
    (println (format "Recording for %s ms with initial buffering for %s ms" ms init-ms))
    (audio/stream! {:ms         ms
                    :init-ms    init-ms
                    :input-type input-type})))
