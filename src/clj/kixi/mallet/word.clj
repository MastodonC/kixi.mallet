(ns kixi.mallet.word
  (:require [clojure.java.io :as io])
  (:import [org.apache.poi.xwpf.usermodel XWPFDocument]
           [org.apache.poi.openxml4j.opc OPCPackage]
           [org.apache.poi.xwpf.extractor XWPFWordExtractor]))

(defn docx->text [f]
  (with-open [in (io/input-stream f)]
    (let [worddoc (new XWPFDocument (OPCPackage/open in))
          extractor (new XWPFWordExtractor worddoc)]
      (.getText extractor))))
