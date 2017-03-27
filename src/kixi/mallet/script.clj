(ns kixi.mallet.script
  (:require [boot.core :as c :refer [deftask]]
            [boot.file :as file]
            [boot.util :refer [info]]
            [kixi.mallet.word :as word]
            [kixi.mallet.pipes :as pipes]
            [kixi.mallet.topics.model :as model]
            [clojure.data.xml :as xml]
            [clojure.data.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [me.raynes.fs :as fs])
  (:import [cc.mallet.classify.tui Text2Vectors]
           [cc.mallet.types InstanceList]
           [cc.mallet.topics.tui TopicTrainer EvaluateTopics HierarchicalLDATUI]
           [cc.mallet.topics ParallelTopicModel]
           [cc.mallet.pipe Pipe SerialPipes
            Target2Label SaveDataInSource Input2CharSequence
            CharSubsequence CharSequenceLowercase CharSequence2TokenSequence
            TokenSequenceRemoveNonAlpha TokenSequenceRemoveStopwords
            TokenSequenceNGrams TokenSequence2FeatureSequence
            FeatureSequence2AugmentableFeatureVector]
           [cc.mallet.pipe.iterator FileIterator]
           [java.io File FileOutputStream ObjectOutputStream]
           [java.nio.charset Charset]))

(defn mallet-arg
  [x]
  (cond
    (vector? x)
    (str/join "," x)
    :else
    (str x)))

(defn options->args [options]
  (->> options
       (mapcat (fn [[k v]]
                 (vector (str "--" (name k)) (mallet-arg v))))
       (into-array String)))

;; Commands

(defn import-dir
  "Import the directory.
  See kixi.mallet.boot for options"
  [opts]
  (let [default-token-regex (re-pattern "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
        directories (into-array File [(:input opts)])
        instantiate (fn [sym]
                      (clojure.lang.Reflector/invokeConstructor (resolve sym) (into-array [])))
        pipes (concat [(Target2Label.)
                       (Input2CharSequence. (.. Charset defaultCharset displayName))]
                      (when-let [string-pipes (:string-pipes opts)]
                        (map instantiate string-pipes))
                      [(CharSequence2TokenSequence. default-token-regex)
                       (TokenSequenceRemoveNonAlpha. true)
                       (when (:remove-stopwords opts)
                         (if-let [file (:extra-stopwords opts)]
                           (TokenSequenceRemoveStopwords. file "UTF-8" true false true)
                           (TokenSequenceRemoveStopwords. false true)))]
                      (when-let [token-pipes (:token-pipes opts)]
                        (map instantiate token-pipes))
                      [(when-let [gram-sizes (:gram-sizes opts)]
                         (TokenSequenceNGrams. (int-array gram-sizes)))
                       (TokenSequence2FeatureSequence.)
                       (when-not (:keep-sequence opts)
                         (FeatureSequence2AugmentableFeatureVector. false))]

                      (when-let [feature-vector-pipes (:feature-vector-pipes opts)]
                        (map instantiate feature-vector-pipes)))
        instance-pipe (SerialPipes. (into-array Pipe (keep identity pipes)))
        instances (InstanceList. instance-pipe)]
    (.addThruPipe instances (FileIterator. directories FileIterator/STARTING_DIRECTORIES true))
    (doto (-> (:output opts)
              (FileOutputStream.)
              (ObjectOutputStream.))
      (.writeObject instances)
      (.close))))

(defn train-topics
  "Train the topics.
  See kixi.mallet.boot for options"
  [opts]
  (-> (options->args opts)
      (TopicTrainer/main)))

(defn train-lda-topics
  "Train the topics using Mallet's HierarchicalLDATUI."
  [opts]
  (-> (options->args opts)
      (HierarchicalLDATUI/main)))

(defn evaluate-topics
  "Evaluate the topics.
  See kixi.mallet.boot for options"
  [opts]
  (-> (options->args opts)
      (EvaluateTopics/main)))

(defn topics-csv
  "TODO: prettify.
  Output a CSV showing the topic allocations"
  [opts]
  (let [model (ParallelTopicModel/read (:model opts))
        instances (InstanceList/load (:input opts))]
    (when-let [phrases (some-> (and (:topics opts) (:xml-topic-phrase-report opts))
                               io/input-stream
                               xml/parse
                               model/xml->topic-phrases)]
      (->> (for [topic (range (inc (apply max (keys phrases))))]
             (let [top-phrases (get phrases topic)
                   top-docs (model/documents-for-topic model topic)]
               (->> (map :source top-docs)
                    (take 3)
                    (concat [(str topic) (:words top-phrases) (:phrases top-phrases)])
                    (map #(str/replace % #"\t" ""))
                    (str/join "\t"))))
           (str/join "\n")
           (spit (:topics opts))))
    (when (:document-topics opts)
      (->> (map-indexed (fn [i instance]
                          (let [probabilities (vec (.getTopicProbabilities model i))
                                topics (->> (map vector (iterate inc 1) probabilities)
                                            (sort-by second >)
                                            (take 3)
                                            (apply concat))
                                {:keys [data name source target]} (bean instance)]
                            (->> (concat [(fs/name name) source] topics)
                                 (str/join "\t"))))
                        instances)
           (str/join "\n")
           (spit (:document-topics opts))))))
