(ns kixi.mallet.word
  (:require [clojure.java.io :as io])
  (:import [org.apache.pdfbox.pdmodel PDDocument]
           [org.apache.pdfbox.text PDFTextStripper]
           [org.apache.poi.xwpf.usermodel XWPFDocument]
           [org.apache.poi.hwpf HWPFDocument]
           [org.apache.poi.openxml4j.opc OPCPackage]
           [org.apache.poi.xwpf.extractor XWPFWordExtractor]
           [org.apache.poi.hwpf.extractor WordExtractor]))

(defn docx->text [f]
  (with-open [in (io/input-stream f)]
    (let [worddoc (new XWPFDocument (OPCPackage/open in))
          extractor (new XWPFWordExtractor worddoc)]
      (.getText extractor))))

(defn pdf->text [f]
  (with-open [in (io/input-stream f)]
    (let [stripper (PDFTextStripper.)
          pdf (PDDocument/load f)
          text (.getText stripper pdf )]
      (.close pdf)
      text)))

(defn doc->text [f]
  (with-open [in (io/input-stream f)]
    (let [doc (HWPFDocument. in)
          extractor (WordExtractor. doc)]
      (.getText extractor))))
