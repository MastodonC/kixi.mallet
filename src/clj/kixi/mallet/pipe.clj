(ns kixi.mallet.pipe
  (:import [cc.mallet.pipe Pipe]
           [org.tartarus.snowball.ext englishStemmer]))

(defn snowball-stemmer-pipe [carrier]
  (let [stemmer (new englishStemmer)]
    (doseq [token (.getData carrier)]
      (.setCurrent stemmer (.getText token))
      (.stem stemmer)
      (.setText token (.getCurrent stemmer)))))

(gen-class :name kixi.mallet.pipe.SnowballStemmer
           :extends cc.mallet.pipe.Pipe
           :prefix snowball-stemmer-)
