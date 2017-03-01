(ns kixi.mallet.boot
  (:require [boot.core :as c :refer [deftask]]
            [boot.file :as file]
            [boot.util :refer [info]]
            [kixi.mallet.word :as word]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [cc.mallet.classify.tui Text2Vectors]))

(defn relative-file? [file]
  (not (.startsWith (str file) "/")))

(defn mallet-arg [fileset x]
  (cond
    (vector? x)
    (str/join "," x)
    (and (instance? File x)
         (relative-file? x))
    (str "target/" x)
    :else
    (str x)))

(defn options->args [fileset options]
  (->> options
       (mapcat (fn [[k v]]
                 (vector (str "--" (name k)) (mallet-arg fileset v))))
       (into-array String)))

(defmacro defmallet [task-name method docstring options]
  `(deftask ~task-name ~docstring ~options
     (let [tmp# (c/tmp-dir!)]
       (c/with-pre-wrap fileset#
         (let [out# (io/file tmp# ~'output)
               in# (io/file tmp# ~'input)]
           (~method (options->args fileset# (merge ~'*opts* {:output out#}))))
         (-> fileset# (c/add-resource tmp#) c/commit!)))))

(defn text-input-meta
  [dir]
  (-> (file-seq dir)
      (->> (filter #(.isFile %))
           (map #(.getPath (file/relative-to dir %))))
      (zipmap (repeat {::kixi-input true}))))

(def text-extractors
  {".docx" word/docx->text})

(defn text-extractor [file]
  (some (fn [[suffix extractor]]
          (when (-> (c/tmp-path file)
                    (.endsWith suffix))
            extractor)) text-extractors))

(defn ->txt-filename
  [filename]
  (str filename ".txt"))

(deftask extract-text
  []
  (let [tmp (c/tmp-dir!)]
    (c/with-pre-wrap fileset
      (c/empty-dir! tmp)
      (let [files (c/by-ext (keys text-extractors) (c/input-files fileset))]
        (doseq [file files
                :let [tmp-file (c/tmp-file file)]]
          (when-let [extractor (text-extractor file)]
            (let [filename (->txt-filename (c/tmp-path file))
                  out-file (io/file tmp filename)]
              (info "Writing %s...\n" filename)
              (doto out-file
                io/make-parents
                (spit (extractor (c/tmp-file file))))
              (file/delete-file tmp-file))))
        (-> fileset
            (c/rm files)
            (c/add-resource tmp)
            (c/add-meta (text-input-meta tmp))
            c/commit!)))))

(defmallet import-dir Text2Vectors/main
  "A boot task exposing Mallet's import-dir command."
  [i input DIR file "The directories containing text files to be classified, one directory per class"
   o output FILE file "Write the instance list to this file; Using - indicates stdout. Default is text.vectors"
   r remove-stopwords bool "If true, remove a default list of common English \"stop words\" from the text. Default is false"
   g gram-sizes INT [int] "Include among the features all n-grams of sizes specified.  For example, to get all unigrams and bigrams, use --gram-sizes 1,2.  This option occurs after the removal of stop words, if removed. Default is 1"
   s keep-sequence bool "If true, final data will be a FeatureSequence rather than a FeatureVector. Default is false"
   p print-output bool "If true, print a representation of the processed data to standard output. This option is intended for debugging. Default is false"])
