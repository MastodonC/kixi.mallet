(ns kixi.mallet.topics.model
  (:require [clojure.string :as str]))

(defn documents-for-topic [model topic]
  (let [sorted-docs (.get (.getTopicDocuments model 10.0) topic)]
    (for [doc sorted-docs
          :let [instance (.. model -data (get (.getID doc)) -instance)]]
      (bean instance))))

(defn xml->topic-phrases [xml]
  (let [topic (fn [t]
                {:id (-> t :attrs :id Long/parseLong)
                 :words (-> t :attrs :titles)
                 :phrases (->> (:content t)
                               (filter #(= :phrase (:tag %)))
                               (mapcat :content)
                               (str/join ", "))})]
    (->> (:content xml)
         (map (comp (juxt :id identity) topic))
         (into {}))))
