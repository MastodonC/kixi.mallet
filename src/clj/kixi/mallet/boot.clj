(ns kixi.mallet.boot
  (:require [boot.core :refer [deftask]])
  (:import [cc.mallet.classify.tui Text2Vectors]))

(defn options->args [options]
  (->> options
       (mapcat (fn [[k v]]
                 (vector (str "--" (name k)) (str v))))
       (into-array String)))

(defmacro defmallet [task-name method docstring options]
  `(deftask ~task-name ~docstring ~options
     (~method (options->args ~'*opts*))))

(defmallet import-dir Text2Vectors/main
  "A task exposing Mallet's import-dir command."
  [c config FILE file "Read command option values from a file. Default is null"
   i input DIR file "The directories containing text files to be classified, one directory per class"
   o output FILE file "Write the instance list to this file; Using - indicates stdout. Default is text.vectors"
   pf use-pipe-from FILE file "Use the pipe and alphabets from a previously created vectors file. Allows the creation, for example, of a test set of vectors that are compatible with a previously created set of training vectors. Default is text.vectors"
   pc preserve-case bool "If true, do not force all strings to lowercase. Default is false"
   rf replacement-files FILE file "files containing string replacements, one per line: 'A B [tab] C' replaces A B with C, 'A B' replaces A B with A_B. Default is (null)"
   df deletion-files FILE file "files containing strings to delete after replacements but before tokenization (ie multiword stop terms). Default is (null)"
   rs remove-stopwords bool "If true, remove a default list of common English \"stop words\" from the text. Default is false"
   sf stoplist-file FILE file "Instead of the default list, read stop words from a file, one per line. Implies --remove-stopwords. Default is null"
   es extra-stopwords FILE file "Read whitespace-separated words from this file, and add them to either the default English stoplist or the list specified by --stoplist-file. Default is null"
   spf stop-pattern-file FILE file "Read regular expressions from a file, one per line. Tokens matching these regexps will be removed. Default is null"
   sh skip-header bool "If true, in each document, remove text occurring before a blank line.  This is useful for removing email or UseNet headers. Default is false"
   ss skip-html bool "If true, remove text occurring inside <...>, as in HTML or SGML. Default is false"
   bf binary-features bool "If true, features will be binary. Default is false"
   gs gram-sizes INT integer "Include among the features all n-grams of sizes specified.  For example, to get all unigrams and bigrams, use --gram-sizes 1,2.  This option occurs after the removal of stop words, if removed. Default is 1"
   ks keep-sequence bool "If true, final data will be a FeatureSequence rather than a FeatureVector. Default is false"
   ksb keep-sequence-bigrams bool "If true, final data will be a FeatureSequenceWithBigrams rather than a FeatureVector. Default is false"
   st save-text-in-source bool "If true, save original text of document in source. Default is false"
   sp string-pipe STRING string "Java code for the constructor of a Pipe to be run as soon as input becomes a CharSequence. Default is null"
   tp token-pipe STRING string "Java code for the constructor of a Pipe to be run as soon as input becomes a TokenSequence. Default is null"
   fvp fv-pipe STRING string "Java code for the constructor of a Pipe to be run as soon as input becomes a FeatureVector. Default is null"
   e encoding STRING string "Character encoding for input file. Default is UTF-8"
   tr token-regex STRING string "Regular expression used for tokenization. Example: \"[\\p{L}\\p{N}_]+|[\\p{P}]+\" (unicode letters, numbers and underscore OR all punctuation). Default is \\p{L}[\\p{L}\\p{P}]+\\p{L}"
   p print-output bool "If true, print a representation of the processed data to standard output. This option is intended for debugging. Default is false"])
