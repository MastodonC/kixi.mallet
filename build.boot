(task-options!
 pom {:project 'kixi.mallet
      :version "0.0.1-SNAPSHOT"})

(set-env!
 :source-paths #{"src/clj" "src/java"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [cc.mallet/mallet "2.0.8"]
                 [com.github.rholder/snowball-stemmer "1.3.0.581.1"]])

(require '[kixi.mallet.boot :refer :all])
