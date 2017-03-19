(ns kixi.mallet.script
  (:require [boot.core :as c :refer [deftask]]
            [boot.file :as file]
            [boot.util :refer [info]]
            [kixi.mallet.word :as word]
            [kixi.mallet.pipes :as pipes]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [me.raynes.fs :as fs])
  (:import [cc.mallet.classify.tui Text2Vectors]
           [cc.mallet.types InstanceList]
           [cc.mallet.topics.tui TopicTrainer EvaluateTopics]
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

(defn print-topics [topics]
  (doseq [[i topic] (map-indexed vector topics)]
    (println (format "== Topic %d ==" i))
    (println (str/join ", " (for [word topic] (str/replace word "_" " "))))
    (println "")))

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
                         (TokenSequenceRemoveStopwords. false true))]
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
    (prn @kixi.mallet.pipes/stemmed-words)
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

(defn view-topics
  "Inspect the topic model.
  See kixi.mallet.boot for options"
  [opts]
  (let [model (ParallelTopicModel/read (:model opts))
        topic-names (-> (.getTopicAlphabet model)
                        (.toString)
                        (clojure.string/split #"\n"))
        top-topics (.getTopWords model (get opts :top-words 20))]
    (print-topics top-topics)))

(defn evaluate-topics
  "Evaluate the topics.
  See kixi.mallet.boot for options"
  [opts]
  (-> (options->args opts)
      (EvaluateTopics/main)))
