(ns hive-ingestor-rfc.mirror-test
  "The local-mirror source for the RFC series.

   Nothing here touches rsync or the network: a mirror is a directory, so the
   test makes one."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-spi.ingest.ports :as sp]
            [hive-ingestor-rfc.mirror :as mirror])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private rfc-2109
  (str "Network Working Group                                     D. Kristol\n"
       "Request for Comments: 2109                                 Bell Labs\n"
       "Category: Standards Track                             February 1997\n"
       "\n\n                   HTTP State Management Mechanism\n\n"
       "Status of this Memo\n\n   This document specifies an Internet standards track protocol.\n\n"
       "Abstract\n\n   This document specifies a way to create stateful sessions.\n\n"
       "1. Terminology\n\n   The terms apply as in RFC 2068.\n\n"
       "2. Overview\n\n   An origin server may send state information.\n"))

(defn- mirror-dir
  [files]
  (let [root (.toFile (Files/createTempDirectory "rfc-mirror-test"
                                                 (into-array FileAttribute [])))]
    (.deleteOnExit root)
    (doseq [[name content] files]
      (let [f (java.io.File. root ^String name)]
        (spit f content)
        (.deleteOnExit f)))
    (str root)))

(deftest only-rfc-documents-count-as-documents
  (testing "a bare rfcN.txt is the document"
    (is (= 2109 (mirror/rfc-number "/mirror/rfc2109.txt")))
    (is (= 9110 (mirror/rfc-number "rfc9110.txt"))))
  (testing "the mirror's other residents are not"
    (is (nil? (mirror/rfc-number "/mirror/rfc2109-errata.txt")))
    (is (nil? (mirror/rfc-number "/mirror/rfc-index.txt")))
    (is (nil? (mirror/rfc-number "/mirror/std1.txt")))
    (is (nil? (mirror/rfc-number nil)))))

(deftest a-mirror-file-is-attributed-to-its-canonical-url
  (testing "so ids and KG nodes agree with anything ingested over HTTP"
    (is (= "https://www.rfc-editor.org/rfc/rfc2109.txt" (mirror/canonical-url 2109)))))

(deftest a-run-is-a-window-over-the-series
  (let [entries (mapv (fn [n] {:rfc/number n :path (str "rfc" n ".txt")}) [9110 2109 7230])]
    (testing "ascending RFC order, always"
      (is (= [2109 7230 9110] (mapv :rfc/number (mirror/window entries {})))))
    (testing "bounded by number"
      (is (= [7230 9110] (mapv :rfc/number (mirror/window entries {:from 7000}))))
      (is (= [2109] (mapv :rfc/number (mirror/window entries {:to 3000})))))
    (testing "and paged, so 10k files can be walked in batches"
      (is (= [7230] (mapv :rfc/number (mirror/window entries {:offset 1 :limit 1}))))
      (is (= [] (mapv :rfc/number (mirror/window entries {:offset 99})))))))

(deftest enumerating-a-mirror-skips-what-is-not-a-document
  (let [dir (mirror-dir {"rfc2109.txt" rfc-2109
                         "rfc2109-errata.txt" "errata"
                         "rfc-index.txt" "index"})
        res (mirror/mirror-entries dir {})]
    (is (r/ok? res))
    (is (= [2109] (mapv :rfc/number (:ok res))))))

(deftest a-mirrored-rfc-parses-as-a-spec-not-as-plain-text
  (let [dir  (mirror-dir {"rfc2109.txt" rfc-2109})
        src  (mirror/rfc-mirror-source dir)
        docs (sp/fetch-documents src {})]
    (is (r/ok? docs))
    (let [doc (first (:ok docs))
          md  (:document/metadata doc)]
      (testing "the source is the canonical URL, not the local path"
        (is (= "https://www.rfc-editor.org/rfc/rfc2109.txt" (:document/source doc))))
      (testing "the RFC rule claimed it"
        (is (= :content/spec (:content-kind md)))
        (is (= "HTTP State Management Mechanism" (:title md)))
        (is (= "RFC 2109" (:tool-name md))))
      (testing "and its structure survived"
        (is (seq (:headings md)))
        (is (some #(= "2. Overview" (:text %)) (:headings md)))))))

(deftest the-source-names-itself-so-a-corpus-gets-its-own-scope
  (is (= "rfc" (sp/source-id (mirror/rfc-mirror-source "/mirror")))))

(deftest a-missing-mirror-is-reported-not-thrown
  (let [src (mirror/rfc-mirror-source "/does/not/exist")]
    (is (r/err? (sp/fetch-documents src {})))
    (is (= :down (:status (sp/source-health src))))))
