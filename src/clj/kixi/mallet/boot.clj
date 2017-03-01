(ns kixi.mallet.boot
  (:require [boot.core :as c :refer [deftask]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [cc.mallet.classify.tui Text2Vectors]))

(defn mallet-arg [x]
  (if (vector? x)
    (str/join "," x)
    (str x)))

(defn options->args [options]
  (->> options
       (mapcat (fn [[k v]]
                 (vector (str "--" (name k)) (mallet-arg v))))
       (into-array String)))

(defmacro defmallet [task-name method docstring options]
  `(deftask ~task-name ~docstring ~options
     (let [tmp# (c/tmp-dir!)]
       (c/with-pre-wrap fileset#
         (let [out# (io/file tmp# ~'output)]
           (~method (options->args (merge ~'*opts* {:output out#}))))
         (-> fileset# (c/add-resource tmp#) c/commit!)))))

(defmallet import-dir Text2Vectors/main
  "A boot task exposing Mallet's import-dir command."
  [i input DIR file "The directories containing text files to be classified, one directory per class"
   o output FILE file "Write the instance list to this file; Using - indicates stdout. Default is text.vectors"
   r remove-stopwords bool "If true, remove a default list of common English \"stop words\" from the text. Default is false"
   g gram-sizes INT [int] "Include among the features all n-grams of sizes specified.  For example, to get all unigrams and bigrams, use --gram-sizes 1,2.  This option occurs after the removal of stop words, if removed. Default is 1"
   s keep-sequence bool "If true, final data will be a FeatureSequence rather than a FeatureVector. Default is false"
   p print-output bool "If true, print a representation of the processed data to standard output. This option is intended for debugging. Default is false"])
