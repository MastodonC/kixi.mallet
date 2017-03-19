(ns kixi.mallet.pipes
  (:require [clojure.string :as str]
            [kixi.mallet.pipe :refer [defpipe]])
  (:import [org.tartarus.snowball.ext englishStemmer]))

;; Utility functions ;;

(def stemmed-words (atom {}))

(def stemmer (new englishStemmer))

(defn stem [text]
  (let [stemmed (doto stemmer
                  (.setCurrent text)
                  (.stem))]
    (swap! stemmed-words update-in [stemmed text] (fnil inc 0))
    stemmed))

(defn stem-data [instance]
  (update instance :data stem))

;; Standard pipe implementations ;;

(defpipe Print
  (map #(doto % prn)))

(defpipe SnowballStemmer
  (map stem-data))

(defpipe SplitParagraphs
  (mapcat (fn [{:keys [data] :as instance}]
            (for [chunk (str/split data #"\n\n+")]
              (assoc instance
                     :data (str/replace chunk #"\s+" " ")
                     :source data)))))

(defpipe RemoveShortInstances
  (remove (fn [{:keys [data]}]
            (<= (count data) 10))))
