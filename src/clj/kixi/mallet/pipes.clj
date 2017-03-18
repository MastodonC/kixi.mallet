(ns kixi.mallet.pipes
  (:require [clojure.string :as str]
            [kixi.mallet.pipe :refer [defpipe]])
  (:import [org.tartarus.snowball.ext englishStemmer]))

;; Utility functions ;;

(def stemmer (new englishStemmer))

(defn stem [text]
  (doto stemmer
    (.setCurrent text)
    (.stem)))

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
