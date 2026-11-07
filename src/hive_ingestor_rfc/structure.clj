(ns hive-ingestor-rfc.structure
  "Pure structural grammar of IETF RFC plaintext.

   Depagination, masthead fields, section headings and section splitting.
   Consumed by the RFC text parser and by section-aware chunking. Owned by
   this addon: the host and the shared ingest kit carry no RFC grammar."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Pagination furniture
;; =============================================================================

(def ^:private page-footer-re
  #"^\S.*\[Page [0-9ivxlcdm]+\]\s*$")

(def ^:private running-header-re
  #"(?i)^(RFC \d+|Internet-Draft|draft-\S+)\s+\S.*\S\s*$")

(defn furniture-line?
  "True when a line is an RFC page header, page footer, or form feed."
  [line]
  (let [line (str/replace line "\f" "")]
    (boolean (or (re-matches page-footer-re line)
                 (re-matches running-header-re line)))))

(defn depaginate
  "Remove RFC page furniture, collapsing the blank runs it leaves behind.

   Every content line keeps its indentation."
  [text]
  (->> (str/split-lines text)
       (remove furniture-line?)
       (map #(str/replace % "\f" ""))
       (reduce (fn [acc line]
                 (if (and (str/blank? line) (str/blank? (peek acc)))
                   acc
                   (conj acc (str/trimr line))))
               [""])
       (drop 1)
       (str/join "\n")
       str/trim))

;; =============================================================================
;; Masthead
;; =============================================================================

(def ^:private front-matter-fields
  {:rfc/number    #"(?i)^Request for Comments:\s*(\d+)\s*$"
   :rfc/category  #"(?i)^Category:\s*(.+?)\s*$"
   :rfc/obsoletes #"(?i)^Obsoletes:\s*(.+?)\s*$"
   :rfc/updates   #"(?i)^Updates:\s*(.+?)\s*$"})

(defn preamble-blocks
  "Blank-line-separated blocks preceding the status section.

   Block 0 is the masthead; block 1 is the document title."
  [lines]
  (->> lines
       (take-while #(not (re-find #"(?i)^(Status of th|Copyright Notice|Abstract\b)" %)))
       (partition-by str/blank?)
       (remove #(every? str/blank? %))
       (mapv vec)))

(defn split-columns
  "Split a masthead line into [left-column right-column].

   A line with no column gap yields [\"\" trimmed-line]."
  [line]
  (let [parts (str/split (str/trim line) #"\s{2,}" 2)]
    (if (= 2 (count parts))
      [(str/trim (first parts)) (str/trim (second parts))]
      ["" (str/trim (first parts))])))

(defn masthead-fields
  "RFC masthead fields taken from the left column of the masthead block."
  [masthead]
  (let [left (map (comp first split-columns) masthead)]
    (reduce-kv (fn [acc k re]
                 (if-let [v (some #(second (re-matches re %)) left)]
                   (assoc acc k v)
                   acc))
               {}
               front-matter-fields)))

(defn masthead-authors
  "Author names, affiliations and publication date from the masthead's right
   column. Returns {:rfc/authors [...] :rfc/date \"...\"}; keys may be absent."
  [masthead]
  (let [right (->> masthead (map (comp second split-columns)) (remove str/blank?) vec)
        date? #(re-matches #"(?i)[A-Z][a-z]+ \d{4}" %)]
    (cond-> {}
      (seq right)        (assoc :rfc/authors (vec (remove date? right)))
      (some date? right) (assoc :rfc/date (last (filter date? right))))))

(defn title
  "The document title — the centered block following the masthead, or nil."
  [lines]
  (some->> (second (preamble-blocks lines))
           (map str/trim)
           (remove str/blank?)
           seq
           (str/join " ")))

(defn abstract
  "The first paragraph of the abstract, or nil.

   Matches a bare `Abstract` heading and a numbered `N. ABSTRACT` section."
  [lines]
  (->> lines
       (drop-while #(not (re-matches #"(?i)\s*(?:\d+(?:\.\d+)*\.?\s+)?abstract\s*:?\s*" %)))
       (drop 1)
       (drop-while str/blank?)
       (take-while (complement str/blank?))
       (map str/trim)
       (str/join " ")
       not-empty))

;; =============================================================================
;; Section headings
;; =============================================================================

(def ^:private numbered-heading-re
  #"^(\d+(?:\.\d+)*)\.?\s+(\S.*?)\s*$")

(def ^:private appendix-heading-re
  #"^((?:Appendix\s+)?[A-Z])\.\s+(\S.*?)\s*$")

(def ^:private unnumbered-headings
  #{"abstract" "status of this memo" "status of memo" "table of contents"
    "copyright notice" "acknowledgements" "acknowledgments" "references"
    "normative references" "informative references" "authors' addresses"
    "author's address" "authors addresses" "security considerations"
    "full copyright statement" "index" "intellectual property"})

(defn toc-line?
  "True for a table-of-contents entry — a dot leader run to a page number."
  [line]
  (boolean (re-find #"\.{3,}\s*\d+\s*$" line)))

(defn heading-at
  "Parse one line as a section heading, or nil.

   Headings start at column 0. Returns {:level :number :text}."
  [line]
  (when-not (or (str/blank? line)
                (toc-line? line)
                (not= line (str/triml line)))
    (let [text (str/trimr line)]
      (or (when-let [[_ number title] (re-matches numbered-heading-re text)]
            {:level  (inc (count (re-seq #"\." number)))
             :number number
             :text   (str number ". " title)})
          (when-let [[_ number title] (re-matches appendix-heading-re text)]
            {:level 1 :number number :text (str number ". " title)})
          (when (contains? unnumbered-headings (str/lower-case text))
            {:level 1 :number nil :text text})))))

(defn headings
  "All section headings in document order."
  [text]
  (->> (str/split-lines text)
       (keep heading-at)
       vec))

(defn split-sections
  "Split depaginated RFC text into sections at column-0 headings.

   Returns [{:heading text-or-nil :level n-or-nil :number s-or-nil :lines [...]}
   ...]; text preceding the first heading forms a leading section with
   :heading nil."
  [text]
  (reduce (fn [acc line]
            (if-let [{:keys [text level number]} (heading-at line)]
              (conj acc {:heading text :level level :number number :lines []})
              (if (empty? acc)
                [{:heading nil :level nil :number nil :lines [line]}]
                (update-in acc [(dec (count acc)) :lines] conj line))))
          []
          (str/split-lines text)))

;; =============================================================================
;; Cross-references
;; =============================================================================

(def ^:private rfc-ref-re
  #"(?i)\bRFC[\s-]?(\d{1,5})\b")

(defn rfc-url
  "Canonical plaintext URL of an RFC number."
  [n]
  (str "https://www.rfc-editor.org/rfc/rfc" n ".txt"))

(defn references
  "Distinct RFC numbers cited by the text, excluding `self`.

   Returns [{:href url :text \"RFC N\"} ...] in ascending numeric order."
  [text self]
  (->> (re-seq rfc-ref-re text)
       (map second)
       distinct
       (remove #(= % (str self)))
       (sort-by parse-long)
       (mapv (fn [n]
               {:href (rfc-url n)
                :text (str "RFC " n)}))))

(defn superseded
  "Links to the RFCs an Obsoletes: field names."
  [obsoletes]
  (->> (re-seq #"\d+" (str obsoletes))
       distinct
       (mapv (fn [n] {:href (rfc-url n) :text (str "RFC " n)}))))
