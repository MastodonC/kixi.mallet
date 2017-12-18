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

           [kixi.mallet TokenSequenceCleaner]
           [java.io File FileOutputStream ObjectOutputStream FileFilter]
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
        token-cleaner-regex (re-pattern "\\p{Alpha}+")
        directory (:input opts)
        instantiate (fn [sym]
                      (clojure.lang.Reflector/invokeConstructor (resolve sym) (into-array [])))
        pipes (concat [(Target2Label.)
                       (Input2CharSequence. (.. Charset defaultCharset displayName))]
                      (when-let [string-pipes (:string-pipes opts)]
                        (map instantiate string-pipes))
                      [(CharSequence2TokenSequence. default-token-regex)
                       (TokenSequenceCleaner. true token-cleaner-regex)
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
        instances (InstanceList. instance-pipe)
        txt-filter (reify FileFilter
                     (accept [this f]
                       (boolean
                        (when (and (.isFile f)
                                   (re-find #".txt$" (.getName f)))
                          (info (format "Importing %s\n" (.getName f)))
                          true))))]
    (.addThruPipe instances (FileIterator. directory txt-filter FileIterator/STARTING_DIRECTORIES true))
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

(defn sq [x]
  (* x x))

(defn sqrt [x]
  (Math/sqrt x))

(defn cosine-similarity [a b]
  (let [counts (apply merge-with +
                      (map
                       (fn [[x y]]
                         {:dot (* x y)
                          :a (sq x)
                          :b (sq y)})
                       (map vector a b)))]
    (try
      (/ (:dot counts)
         (* (sqrt (:a counts))
            (sqrt (:b counts))))
      (catch Exception e 0.0))))

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
                    (concat [(str (inc topic)) (:words top-phrases) (:phrases top-phrases)])
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

(defn name-rollup-csv
  [opts]
  (let [model (ParallelTopicModel/read (:model opts))
        instances (InstanceList/load (:input opts))]
    (when (:document-topics opts)
      (->> (map-indexed vector instances)
           (reduce (fn [coll [i instance]]
                     (let [probabilities (vec (.getTopicProbabilities model i))
                           {:keys [data name source target]} (bean instance)]
                       (update coll name #(if %1 (map + %1 %2) %2) probabilities))) {})
           (map (fn [[name probabilities]]
                  (let [sum (apply + probabilities)
                        topics (->> (map #(/ % sum) probabilities)
                                    (map vector (iterate inc 1))
                                    (sort-by second >)
                                    (take 3)
                                    (apply concat))]
                    (->> (concat [name] topics)
                         (str/join "\t")))))
           (str/join "\n")
           (spit (:document-topics opts))))))

(defn topics-graph
  [opts]
  (let [model (ParallelTopicModel/read (:model opts))
        instances (InstanceList/load (:input opts))
        documents (->> (map-indexed vector instances)
                       (reduce (fn [coll [i instance]]
                                 (let [probabilities (vec (.getTopicProbabilities model i))
                                       {:keys [data name source target]} (bean instance)]
                                   (update coll (fs/name name) #(if %1 (map + %1 %2) %2) probabilities))) {})
                       (reduce (fn [coll [k ps]]
                                 (let [sum (apply + ps)]
                                   (assoc coll k (map #(/ % sum) ps)))) {})
                       (map-indexed vector))
        topics (->> (map #(vec (.getTopicProbabilities model %)) (range (count instances)))
                    (apply map vector)
                    (map-indexed vector))]
    (when (:documents-net opts)
      (->> (str (format "*Vertices %d\n" (count documents))
                (->> documents
                     (map
                      (fn [[i [title p]]]
                        (format "%d \"%s\" 0.0 0.0 0.0\n" (inc i) title)))
                     (str/join))
                "*Arcs"
                (->> (for [[i [a ap]] documents
                           [j [b bp]] documents
                           :when (not= i j)]
                       (format "%d %d %f\n" (inc i) (inc j) (cosine-similarity ap bp)))
                     (str/join)))
           (spit (:documents-net opts))))
    (when (:topics-net opts)
      (->> (str (format "*Vertices %d\n" (count topics))
                (->> topics
                     (map
                      (fn [[i p]]
                        (format "%d \"%s\" 0.0 0.0 0.0\n" (inc i) (inc i))))
                     (str/join))
                "*Arcs"
                (->> (for [[i ap] topics
                           [j bp] topics
                           :when (not= i j)]
                       (format "%d %d %f\n" (inc i) (inc j) (cosine-similarity ap bp)))
                     (str/join)))
           (spit (:topics-net opts))))))
