(task-options!
 pom {:project 'kixi/mallet
      :version "0.0.1-SNAPSHOT"}
 push {:repo "clojars"})

(set-env!
 :source-paths #{"src"}
 :resource-paths #{"src" "resources"}
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [cc.mallet/mallet "2.0.8"]
                 [me.raynes/fs "1.4.6"]
                 [org.apache.pdfbox/pdfbox "2.0.5"]
                 [org.apache.poi/poi "3.15"]
                 [org.apache.poi/poi-scratchpad "3.15"]
                 [org.apache.poi/poi-ooxml "3.15"]
                 [com.github.rholder/snowball-stemmer "1.3.0.581.1"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/data.zip "0.1.1"]]
 :repositories #(conj % ["clojars" {:url "https://clojars.org/repo/"
                                    :username (System/getenv "CLOJARS_USER")
                                    :password (System/getenv "CLOJARS_PASS")}]))

;; (require '[kixi.mallet.boot :refer :all])

(deftask release-locally []
  (comp (aot :namespace #{'kixi.mallet.pipes})
        (pom)
        (jar)
        (install)))

(deftask release
  []
  (comp (aot :namespace #{'kixi.mallet.pipes})
        (pom)
        (jar)
        (push)))
