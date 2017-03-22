(ns kixi.mallet.pipes
  (:require [clojure.string :as str]
            [kixi.mallet.pipe :refer [defpipe]])
  (:import [org.tartarus.snowball.ext englishStemmer]
           [cc.mallet.extract StringSpan]
           [cc.mallet.types TokenSequence Token]))

;; Utility functions ;;

(def stemmed-words (atom {}))

(def stemmer (new englishStemmer))

(defn set-properties [obj properties]
  (reduce #(doto % (.setProperty ))))

(defn stem-tokens!
  "MUTATION ALERT: updates tokens in-place"
  [tokens]
  (doseq [token tokens
          :let [stemmed (-> stemmer
                            (doto (.setCurrent (.getText token)) (.stem))
                            (.getCurrent))]]
    (do (swap! stemmed-words update-in [stemmed (.getText token)] (fnil inc 0))
        (doto token (.setText stemmed))))
  tokens)

(defn stem-data [instance]
  (update instance :data stem-tokens!))

;; Basic pipe implementations ;;

(defpipe Print
  (map #(doto % prn)))

(defpipe SnowballStemmer
  (map stem-data))
