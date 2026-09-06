(ns hive-ingestor-rfc.index-test
  "The registry: what a document cannot know about itself."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-ingestor-rfc.index :as index]
            [hive-ingestor-rfc.mirror :as mirror]
            [hive-spi.ingest.ports :as sp])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private index-xml
  (str "<rfc-index>"
       "<rfc-entry><doc-id>RFC2109</doc-id><title>HTTP State Management Mechanism</title>"
       "<current-status>HISTORIC</current-status>"
       "<obsoleted-by><doc-id>RFC2965</doc-id></obsoleted-by></rfc-entry>"
       "<rfc-entry><doc-id>RFC2616</doc-id><title>HTTP/1.1</title>"
       "<current-status>DRAFT STANDARD</current-status>"
       "<obsoleted-by><doc-id>RFC7230</doc-id><doc-id>RFC7231</doc-id></obsoleted-by>"
       "<updated-by><doc-id>RFC8615</doc-id></updated-by></rfc-entry>"
       "<rfc-entry><doc-id>RFC9110</doc-id><title>HTTP Semantics</title>"
       "<current-status>INTERNET STANDARD</current-status></rfc-entry>"
       "</rfc-index>"))

(def ^:private rfc-2109
  (str "Network Working Group                                     D. Kristol\n"
       "Request for Comments: 2109                                 Bell Labs\n"
       "Category: Standards Track                             February 1997\n"
       "\n\n                   HTTP State Management Mechanism\n\n"
       "Abstract\n\n   A way to create stateful sessions.\n\n"
       "1. Terminology\n\n   Terms apply as in RFC 2068.\n"))

(defn- mirror-dir
  [files]
  (let [root (.toFile (Files/createTempDirectory "rfc-index-test"
                                                 (into-array FileAttribute [])))]
    (.deleteOnExit root)
    (doseq [[name content] files]
      (let [f (java.io.File. root ^String name)]
        (spit f content)
        (.deleteOnExit f)))
    (str root)))

(deftest the-registry-parses-into-records-per-rfc
  (let [dir (mirror-dir {"rfc-index.xml" index-xml})
        res (index/parse (str dir "/rfc-index.xml"))]
    (is (r/ok? res))
    (let [registry (:ok res)]
      (is (= #{2109 2616 9110} (set (keys registry))))
      (testing "status and forward supersession, which the .txt cannot carry"
        (is (= "HISTORIC" (:status (get registry 2109))))
        (is (= [2965] (:obsoleted-by (get registry 2109))))
        (is (= [7230 7231] (:obsoleted-by (get registry 2616))))
        (is (= [8615] (:updated-by (get registry 2616)))))
      (testing "a current document is not marked superseded"
        (is (= [] (:obsoleted-by (get registry 9110))))))))

(deftest facets_mark_a_superseded_document_at_read_time
  (let [registry (:ok (index/parse (str (mirror-dir {"rfc-index.xml" index-xml}) "/rfc-index.xml")))]
    (testing "a historic document says so, and says by what"
      (let [tags (index/facets (get registry 2109))]
        (is (some #{"status:historic"} tags))
        (is (some #{"obsoleted-by:2965"} tags))
        (is (some #{"superseded"} tags))))
    (testing "a current document carries its status and no supersession"
      (let [tags (index/facets (get registry 9110))]
        (is (some #{"status:internet-standard"} tags))
        (is (not-any? #(re-find #"^obsoleted-by:" %) tags))
        (is (not-any? #{"superseded"} tags))))
    (testing "an RFC the registry does not mention gets nothing invented for it"
      (is (= [] (index/facets nil))))))

(deftest registry-facets-reach-the-ingested-document
  (let [dir  (mirror-dir {"rfc2109.txt" rfc-2109 "rfc-index.xml" index-xml})
        docs (sp/fetch-documents (mirror/rfc-mirror-source dir) {})
        md   (:document/metadata (first (:ok docs)))]
    (testing "the document carries the registry's view"
      (is (= "HISTORIC" (:rfc/status md)))
      (is (= [2965] (:rfc/obsoleted-by md))))
    (testing "and it reaches :facets, the generic seam a host emits on document AND chunks"
      (let [facets (:facets md)]
        (is (some #{"status:historic"} facets))
        (is (some #{"obsoleted-by:2965"} facets))))))

(deftest a-mirror-without-a-registry-still-ingests
  (let [dir  (mirror-dir {"rfc2109.txt" rfc-2109})
        docs (sp/fetch-documents (mirror/rfc-mirror-source dir) {})
        md   (:document/metadata (first (:ok docs)))]
    (testing "no status is invented"
      (is (nil? (:rfc/status md)))
      (is (not-any? #(re-find #"^status:" %) (:facets md))))
    (testing "but the document is still there"
      (is (= 1 (count (:ok docs)))))))
