(ns hive-ingestor-rfc.structure-test
  "Structural grammar of RFC plaintext.

   Properties:
   - Depagination removes only page furniture, never content or indentation
   - Depagination is idempotent
   - Headings anchor at column 0 and never match TOC entries
   - Section splitting partitions every content line exactly once"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-ingestor-rfc.structure :as rfc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def sample-rfc
  "A miniature RFC carrying every structural feature."
  (slurp (io/resource "hive_ingestor_rfc/fixtures/sample-rfc.txt")))

;; =============================================================================
;; Depagination
;; =============================================================================

(deftest depaginate-removes-page-furniture
  (let [out (rfc/depaginate sample-rfc)]
    (testing "page footers, running headers and form feeds are gone"
      (is (not (str/includes? out "[Page 1]")))
      (is (not (str/includes? out "\f")))
      (is (not (re-find #"(?m)^RFC 9999\s+Example Protocol" out))))
    (testing "content survives with its indentation"
      (is (str/includes? out "       Indented syntax block."))
      (is (str/includes? out "2.1. Syntax"))
      (is (str/includes? out "   Semantics text, see RFC 2616 and RFC 1945.")))))

(deftest depaginate-is-idempotent
  (let [once (rfc/depaginate sample-rfc)]
    (is (= once (rfc/depaginate once)))))

(defspec depaginate-never-adds-lines 100
  (prop/for-all [lines (gen/vector gen/string-ascii 0 40)]
    (let [text (str/join "\n" lines)]
      (<= (count (str/split-lines (rfc/depaginate text)))
          (max 1 (count lines))))))

(defspec depaginate-preserves-non-furniture-lines 100
  (prop/for-all [lines (gen/vector (gen/such-that (complement rfc/furniture-line?)
                                                  gen/string-alphanumeric)
                                   1 20)]
    (let [kept (set (remove str/blank? (str/split-lines (rfc/depaginate (str/join "\n" lines)))))]
      (every? #(or (str/blank? %) (contains? kept (str/trimr %))) lines))))

;; =============================================================================
;; Masthead and front matter
;; =============================================================================

(deftest masthead-fields-come-from-the-left-column
  (let [lines    (str/split-lines (rfc/depaginate sample-rfc))
        masthead (first (rfc/preamble-blocks lines))
        fields   (rfc/masthead-fields masthead)]
    (is (= "9999" (:rfc/number fields)))
    (is (= "Standards Track" (:rfc/category fields))
        "the right column must not bleed into the field value")
    (is (= "9998" (:rfc/obsoletes fields)))
    (is (nil? (:rfc/updates fields)))))

(deftest masthead-authors-come-from-the-right-column
  (let [lines    (str/split-lines (rfc/depaginate sample-rfc))
        masthead (first (rfc/preamble-blocks lines))
        authors  (rfc/masthead-authors masthead)]
    (is (= ["A. Author" "Example Corp" "B. Second"] (:rfc/authors authors)))
    (is (= "January 2020" (:rfc/date authors)))))

(deftest title-is-the-block-after-the-masthead
  (let [lines (str/split-lines (rfc/depaginate sample-rfc))]
    (is (= "Example Protocol for Testing" (rfc/title lines)))))

(deftest abstract-is-the-first-paragraph
  (let [lines (str/split-lines (rfc/depaginate sample-rfc))]
    (is (= "This protocol does nothing at all. It exists only for tests."
           (rfc/abstract lines)))))

(deftest abstract-matches-a-numbered-abstract-section
  (let [lines ["1.  ABSTRACT" "" "   Numbered abstract body." "" "2.  NEXT"]]
    (is (= "Numbered abstract body." (rfc/abstract lines)))))

;; =============================================================================
;; Headings
;; =============================================================================

(deftest heading-at-classification
  (testing "numbered headings carry a level equal to their depth"
    (is (= {:level 1 :number "1" :text "1. Introduction"}
           (rfc/heading-at "1. Introduction")))
    (is (= {:level 2 :number "2.1" :text "2.1. Syntax"}
           (rfc/heading-at "2.1. Syntax")))
    (is (= 3 (:level (rfc/heading-at "3.2.1 General Syntax")))))
  (testing "well-known unnumbered headings are recognised"
    (is (= {:level 1 :number nil :text "Abstract"} (rfc/heading-at "Abstract")))
    (is (some? (rfc/heading-at "Security Considerations"))))
  (testing "appendices are level 1"
    (is (= 1 (:level (rfc/heading-at "A. Internet Media Type")))))
  (testing "non-headings"
    (is (nil? (rfc/heading-at "   1. Indented is body text")))
    (is (nil? (rfc/heading-at "   1. Introduction ..........................1")))
    (is (nil? (rfc/heading-at "")))
    (is (nil? (rfc/heading-at "Ordinary prose that runs on and on.")))))

(deftest headings-skips-the-table-of-contents
  (let [texts (mapv :text (rfc/headings (rfc/depaginate sample-rfc)))]
    (is (= ["Status of this Memo" "Abstract" "Table of Contents"
            "1. Introduction" "2. Details" "2.1. Syntax" "2.2. Semantics"]
           texts))
    (is (= 1 (count (filter #{"1. Introduction"} texts)))
        "the TOC entry must not produce a second heading")))

(defspec heading-at-requires-column-zero 200
  (prop/for-all [indent (gen/fmap inc gen/nat)
                 title  (gen/not-empty gen/string-alphanumeric)]
    (nil? (rfc/heading-at (str (str/join (repeat indent " ")) "1. " title)))))

;; =============================================================================
;; Section splitting
;; =============================================================================

(deftest split-sections-partitions-the-document
  (let [text     (rfc/depaginate sample-rfc)
        sections (rfc/split-sections text)]
    (testing "one section per heading, plus the pre-heading masthead"
      (is (= 8 (count sections)))
      (is (nil? (:heading (first sections)))))
    (testing "every content line lands in exactly one section"
      (is (= (count (str/split-lines text))
             (reduce + (map (fn [{:keys [heading lines]}]
                              (+ (if heading 1 0) (count lines)))
                            sections)))))
    (testing "bodies keep indentation"
      (is (some #(str/includes? (str/join "\n" (:lines %)) "       Indented syntax block.")
                sections)))))

(defspec split-sections-loses-no-line 50
  (prop/for-all [lines (gen/vector gen/string-alphanumeric 1 30)]
    (let [text     (str/join "\n" lines)
          sections (rfc/split-sections text)
          emitted  (reduce + (map (fn [{:keys [heading lines]}]
                                    (+ (if heading 1 0) (count lines)))
                                  sections))]
      (= (count (str/split-lines text)) emitted))))

;; =============================================================================
;; Cross-references
;; =============================================================================

(deftest references-are-distinct-sorted-and-exclude-self
  (let [refs (rfc/references (rfc/depaginate sample-rfc) "9999")]
    (is (= ["RFC 1945" "RFC 2616"] (mapv :text refs)))
    (is (= "https://www.rfc-editor.org/rfc/rfc1945.txt" (:href (first refs))))))

(deftest references-tolerate-a-nil-self
  (is (= ["RFC 2616"] (mapv :text (rfc/references "see RFC 2616 and RFC-2616" nil)))))
