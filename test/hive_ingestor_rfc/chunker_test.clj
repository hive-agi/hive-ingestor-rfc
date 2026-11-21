(ns hive-ingestor-rfc.chunker-test
  "Section chunking of RFC plaintext, owned by the addon."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
            [hive-ingestor-rfc.chunker :as chunker]
            [hive-ingestor-rfc.structure :as rfc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private sample-rfc
  (rfc/depaginate (slurp (io/resource "hive_ingestor_rfc/fixtures/sample-rfc.txt"))))

(defn- chunks-of [text]
  (:ok (chunker/chunk-spec {:document/content text :document/source "test"} {})))

(deftest spec-documents-chunk-by-rfc-section
  (let [chunks (chunks-of sample-rfc)]
    (testing "sections become chunks carrying their heading"
      (is (seq chunks))
      (is (some #(= "2.1. Syntax" (:heading %)) chunks))
      (is (= 2 (:level (first (filter #(= "2.1. Syntax" (:heading %)) chunks))))))
    (testing "navigation sections are dropped"
      (is (not-any? #(= "Table of Contents" (:heading %)) chunks)))
    (testing "no chunk is a bare heading: an empty section folds forward"
      (is (not-any? #(< (count (:content %)) 20) chunks))
      (is (some #(and (= "2.1. Syntax" (:heading %))
                      (str/includes? (:content %) "2. Details"))
                chunks)))
    (testing "chunk content preserves indentation"
      (is (some #(str/includes? (:content %) "       Indented syntax block.") chunks)))
    (testing "indexes are dense and ids unique"
      (is (= (range (count chunks)) (map :index chunks)))
      (is (apply distinct? (map :id chunks))))))

(deftest oversized-sections-are-subdivided
  (let [para   (str/join " " (repeat 120 "word"))
        text   (str "1. Big Section\n\n" (str/join "\n\n" (repeat 12 para)))
        chunks (chunks-of text)]
    (is (> (count chunks) 1))
    (is (every? #(<= (count (:content %)) chunker/section-max-chars) chunks))
    (is (every? #(= "1. Big Section" (:heading %)) chunks))))

(deftest text-without-sections-still-chunks
  (is (seq (chunks-of "   just prose\n   with no headings"))))

(deftest blank-content-chunks-to-nothing
  (is (= [] (chunks-of "")))
  (is (= [] (chunks-of "   \n  "))))

(deftest the-document-title-rides-on-every-chunk
  (let [chunks (:ok (chunker/chunk-spec {:document/content sample-rfc
                                         :document/source  "s"
                                         :document/metadata {:title "RFC 9999"}}
                                        {}))]
    (is (every? #(= "RFC 9999" (:document/title %)) chunks))))

(defspec chunk-spec-is-total-and-bounded 100
  (prop/for-all [lines (gen/vector gen/string-ascii 0 40)]
    (let [result (chunker/chunk-spec {:document/content (str/join "\n" lines)
                                      :document/source "g"} {})]
      (and (r/ok? result)
           (every? #(<= (count (:content %)) chunker/section-max-chars) (:ok result))))))
