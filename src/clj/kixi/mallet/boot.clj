(ns kixi.mallet.boot
  (:require [boot.core :as c :refer [deftask]]
            [boot.file :as file]
            [boot.util :refer [info]]
            [kixi.mallet.word :as word]
            [kixi.mallet.pipes :as pipes]
            [kixi.mallet.script :as script]
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

;; Utility functions ;;

(defn by-prefix [prefix files]
  (-> (str "^" prefix)
      (re-pattern)
      (vector)
      (c/by-re files)))

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

(defn update-vals-for-keys [coll f keys]
  (reduce (fn [coll k]
            (update coll k f)) coll keys))

(defn shared-parent-directory
  "Find the shared parent directory with the longest path"
  [file1 file2]
  (->> (clojure.set/intersection (set (conj (fs/parents file1) file1))
                                 (set (conj (fs/parents file2) file2)))
       (apply max-key (comp count str))))

(defn with-fileset-wrapping
  "Provide a wapper around a function which operates on files.
  This wrapper takes care of creating a temp directory
  and committing changes back to the fileset.
  For this to work we need to know which keys in the opts map
  correspond to input or output files."
  [f opts {:keys [in out in-dir]}]
  (c/with-pre-wrap fileset
    (let [tmp (c/tmp-dir!)
          infile (fn [input]
                   (some-> (c/by-name [(str input)] (c/input-files fileset))
                           (first)
                           (c/tmp-file)))
          outfile (fn [output]
                    (doto (io/file tmp output)
                      io/make-parents))
          indir (fn [input]
                  (->>  (c/by-re [#"docs"] (c/input-files fileset))
                        (c/not-by-name [".DS_Store"])
                        (map c/tmp-file)
                        (reduce shared-parent-directory)))
          opts (-> (update-vals-for-keys opts infile in)
                   (update-vals-for-keys indir in-dir)
                   (update-vals-for-keys outfile out))]
      (f opts)
      (-> fileset
          (c/add-resource tmp)
          (c/commit!)))))

;; Tasks ;;

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

(deftask import-dir
  "A boot task which mirrors Mallet's import-dir command."
  [i input DIR file "The directory containing text files to be classified, one directory per class"
   o output FILE file "Write the instance list to this file; Using - indicates stdout. Default is text.vectors"
   r remove-stopwords bool "If true, remove a default list of common English \"stop words\" from the text. Default is false"
   g gram-sizes INT [int] "Include among the features all n-grams of sizes specified.  For example, to get all unigrams and bigrams, use --gram-sizes 1,2.  This option occurs after the removal of stop words, if removed. Default is 1"
   s keep-sequence bool "If true, final data will be a FeatureSequence rather than a FeatureVector. Default is false"
   p string-pipes NAME [sym] "An ordered sequence of string pipes to apply"
   t token-pipes NAME [sym] "An ordered sequence of token pipes to apply"
   v feature-vector-pipes NAME [sym] "An ordered sequence of feature vector pipes to apply"]

  (with-fileset-wrapping
    script/import-dir *opts*
    {:in-dir #{:input}
     :out #{:output}}))

(deftask train-topics
  "A boot task exposing Mallet's train-topics command."
  [i input FILE file "The filename from which to read the list of training instances.  Use - for stdin. The instances must be FeatureSequence or FeatureSequenceWithBigrams, not FeatureVector. Default is null"
   o output-model FILE file "The filename in which to write the binary topic model at the end of the iterations.  By default this is null, indicating that no file will be written. Default is null"
   s output-state FILE file "The filename in which to write the Gibbs sampling state after at the end of the iterations.  By default this is null, indicating that no file will be written. Default is null"
   k num-topics NUMBER int "The number of topics to fit. Default is 10"
   n output-model-interval INT int "The number of iterations between writing the model (and its Gibbs sampling state) to a binary file. You must also set the --output-model to use this option, whose argument will be the prefix of the filenames. Default is 0"
   x output-state-interval INT int "The number of iterations between writing the sampling state to a text file. You must also set the --output-state to use this option, whose argument will be the prefix of the filenames. Default is 0"
   a alpha DECIMAL float "SumAlpha parameter: sum over topics of smoothing over doc-topic distributions. alpha_k = [this value] / [num topics]. Default is 5.0"
   b beta DECIMAL float "Beta parameter: smoothing parameter for each topic-word. beta_w = [this value]. Default is 0.01"
   e evaluator-filename FILE file "A held-out likelihood evaluator for new documents"
   t show-topics-interval INT int "The number of iterations between printing a brief summary of topics so far"]

  (with-fileset-wrapping
    script/train-topics *opts*
    {:in #{:input}
     :out #{:output-model}}))

(deftask view-topics
  [ ;;i input FILE file "The filename from which to read the list of instances"
   m model FILE file "The filename from which to read the model"
   n top-words INT int "The number of top words to show"]
  (with-fileset-wrapping
    script/view-topics *opts*
    {:in #{:model}}))

(deftask evaluate-topics
  "A boot task exposing Mallet's evaluate-topics"
  [e evaluator FILE file "Evaluator filename"
   i input FILE file "The filename from which to read the list of instances"
   d output-doc-probs FILE file "The filename in which to write the interred log probabilities per document"
   p output-prob FILE file "The filename in which to write the inferred log probability of the testing set"
   k num-particles INT int "The number of particles to use in left-to-right evaluation"
   w show-words bool "If true, print the log likelihood of each indvidual token"
   r use-resampling bool "Whether to resample topics in left-to-right evaluation. Resampling is more accurate, but leads to quadratic scaling in the length of the documents"
   n num-iterations INT int "The number of iterations of Gibbs sampling"
   s sample-interval INT int "The number of iterations between saved samples"
   b burn-in INT int "The number of iterations before the first sample is saved"
   x random-seed INT int "The random seed for the Gibbs sampler"]

  (with-fileset-wrapping
    script/evaluate-topics *opts*
    {:in #{:input :evaluator}
     :out #{:output-doc-probs :output-prob}}))
