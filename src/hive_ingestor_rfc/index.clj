(ns hive-ingestor-rfc.index
  "The RFC registry: what a document cannot know about itself.

   An RFC's masthead carries Obsoletes:/Updates: — edges pointing BACKWARD.
   Whether the document has since been obsoleted, and what its current status
   is, exist only in the registry (rfc-index.xml). A parser reading the .txt
   can build supersession edges but can never mark a document HISTORIC, so
   retrieval over the corpus answers present-tense questions confidently and
   wrongly.

   This namespace turns the registry into facets the ingest pipeline attaches
   to both the document and its chunks."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r]))

(def index-file-name
  "The registry file inside an rsync mirror of the series."
  "rfc-index.xml")

(defn- tag-of [element] (name (:tag element)))

(defn- text-of
  [element]
  (some->> (:content element)
           (filter string?)
           (str/join)
           str/trim
           not-empty))

(defn- children
  [element tag]
  (filter #(and (map? %) (= tag (tag-of %))) (:content element)))

(defn- doc-numbers
  "The RFC numbers listed under CHILD-TAG of an entry, e.g. obsoleted-by."
  [entry child-tag]
  (into []
        (keep (fn [id]
                (some-> (text-of id)
                        (->> (re-find #"(?i)RFC0*(\d+)"))
                        second
                        parse-long)))
        (mapcat #(children % "doc-id") (children entry child-tag))))

(defn entry->record
  "One <rfc-entry> as {:number :status :obsoleted-by :updated-by :title}."
  [entry]
  (when-let [number (some-> (first (children entry "doc-id"))
                            text-of
                            (->> (re-find #"(?i)RFC0*(\d+)"))
                            second
                            parse-long)]
    {:number       number
     :title        (some-> (first (children entry "title")) text-of)
     :status       (some-> (first (children entry "current-status")) text-of)
     :obsoleted-by (doc-numbers entry "obsoleted-by")
     :updated-by   (doc-numbers entry "updated-by")}))

(defn parse
  "Parse rfc-index.xml at PATH into {rfc-number -> record}.

   Returns Result. The registry is one 10 MB file read once per run, not per
   document."
  [path]
  (r/try-effect* :rfc-index/parse-failed
    (with-open [in (io/input-stream (io/file path))]
      (let [root ((requiring-resolve 'clojure.xml/parse) in)]
        (into {}
              (keep (fn [entry]
                      (when-let [record (entry->record entry)]
                        [(:number record) record])))
              (children root "rfc-entry"))))))

(defn- status-slug
  [status]
  (some-> status str/lower-case str/trim (str/replace #"[^a-z0-9]+" "-") not-empty))

(defn facets
  "Registry facets for one RFC: its current status and what supersedes it.

   `status:historic` and `obsoleted-by:6265` are the tags that let a hit be
   marked superseded at read time. Returns [] when the registry says nothing."
  [record]
  (if-not record
    []
    (into (if-let [slug (status-slug (:status record))]
            [(str "status:" slug)]
            [])
          cat
          [(map #(str "obsoleted-by:" %) (:obsoleted-by record))
           (map #(str "updated-by:" %) (:updated-by record))
           (when (seq (:obsoleted-by record)) ["superseded"])])))

(defn enrich-metadata
  "Document metadata plus the registry's view of this RFC.

   :facets is the host's generic seam — it emits them on the document entry
   and on every chunk of it, so a retrieval hit can be marked superseded
   without the ingestor ever learning what an RFC is."
  [metadata record]
  (let [tags (facets record)]
    (cond-> metadata
      (seq tags)             (update :facets (fnil into []) tags)
      (:status record)       (assoc :rfc/status (:status record))
      (seq (:obsoleted-by record)) (assoc :rfc/obsoleted-by (:obsoleted-by record))
      (seq (:updated-by record))   (assoc :rfc/updated-by (:updated-by record)))))
