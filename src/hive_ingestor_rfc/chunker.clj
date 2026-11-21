(ns hive-ingestor-rfc.chunker
  "Section-aware chunking of RFC plaintext: one chunk per section.

   A pure chunk-fn with the host's content-kind :chunk-fn contract,
   (fn [document opts] -> Result<[chunk]>), registered for :content/spec on
   addon initialize!. Sections wider than the section bound are subdivided at
   paragraph boundaries; text with no RFC headings falls back to one chunk per
   paragraph pack, so the function is total."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-ingestor-rfc.structure :as rfc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def section-max-chars
  "Widest chunk this chunker emits."
  4000)

(defn- make-chunk
  [source content index]
  {:id      (str (random-uuid))
   :content content
   :index   index
   :start   0
   :end     (count content)
   :source  source})

(defn- section-text
  "One section as trimmed text: breadcrumb headings of the empty parent
   sections above it, then its own heading, then its body."
  [{:keys [heading breadcrumb lines]}]
  (str/trim (str/join "\n" (concat breadcrumb (when heading [heading]) lines))))

(def ^:private navigation-headings
  #{"table of contents" "index"})

(defn- prune-navigation
  "Drop sections that only reproduce navigation already carried by the
   document's :headings metadata."
  [sections]
  (remove #(contains? navigation-headings
                      (some-> (:heading %) str/lower-case str/trim))
          sections))

(defn- fold-empty-headings
  "Fold a heading whose section has no body into the next section as a
   breadcrumb, so no chunk is a bare heading."
  [sections]
  (:sections
   (reduce (fn [{:keys [pending sections]} {:keys [heading lines] :as section}]
             (if (str/blank? (str/join lines))
               {:pending (cond-> pending heading (conj heading)) :sections sections}
               {:pending  []
                :sections (conj sections (assoc section :breadcrumb pending))}))
           {:pending [] :sections []}
           sections)))

(defn- hard-cut
  [text max-chars]
  (if (<= (count text) max-chars)
    [text]
    (mapv #(subs text % (min (+ % max-chars) (count text)))
          (range 0 (count text) max-chars))))

(defn pack-paragraphs
  "Group paragraphs into pieces of at most MAX-CHARS, splitting no paragraph
   that already fits."
  [text max-chars]
  (if (<= (count text) max-chars)
    [text]
    (->> (str/split text #"\n\n")
         (reduce (fn [acc para]
                   (let [current (peek acc)]
                     (if (and current
                              (<= (+ (count current) 2 (count para)) max-chars))
                       (conj (pop acc) (str current "\n\n" para))
                       (conj acc para))))
                 [])
         (mapcat #(hard-cut % max-chars))
         vec)))

(defn section-chunks
  "TEXT as one chunk per RFC section. Returns [chunk]."
  [text source]
  (let [sections (rfc/split-sections text)
        pieces   (if (some :heading sections)
                   (->> sections
                        prune-navigation
                        fold-empty-headings
                        (mapcat (fn [{:keys [heading level number] :as section}]
                                  (map (fn [piece] {:heading heading :level level
                                                    :number number :content piece})
                                       (pack-paragraphs (section-text section)
                                                        section-max-chars)))))
                   (map (fn [piece] {:content piece})
                        (pack-paragraphs (str/trim text) section-max-chars)))]
    (->> pieces
         (remove #(str/blank? (:content %)))
         (map-indexed (fn [idx {:keys [heading level number content]}]
                        (cond-> (make-chunk source content idx)
                          heading (assoc :heading heading)
                          level   (assoc :level level)
                          number  (assoc :number number))))
         vec)))

(defn chunk-spec
  "The :chunk-fn registered for :content/spec. Returns Result<[chunk]>."
  [document _opts]
  (let [text  (:document/content document)
        title (get-in document [:document/metadata :title])]
    (if (str/blank? text)
      (r/ok [])
      (r/try-effect* :ingest/chunking-failed
                     (cond->> (section-chunks text (:document/source document))
                       (not (str/blank? (str title)))
                       (mapv #(assoc % :document/title title)))))))
