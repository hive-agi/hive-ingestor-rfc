(ns hive-ingestor-rfc.mirror
  "ISource over a local rsync mirror of the RFC series.

   Bulk access to the series is rsync, not HTTP:
     rsync -avz --delete rsync.rfc-editor.org::rfcs-text-only <dir>

   Only the transport differs from the web source — the same rule chain, the
   same RfcTextParser and the same Document shape are reused through
   web-docs/body->document, and :document/source is the canonical
   rfc-editor URL, so document ids and cross-document KG edges agree with
   anything ingested over HTTP.

   The series is ~10k files; a run is a WINDOW over it (:from, :to, :offset,
   :limit). Re-running a window is an upsert, not a duplicate, because chunk
   and document ids are derived from the source URL — that is the resumable
   story: crash, re-run the window, continue."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-spi.ingest.ports :refer [ISource ISourceHealth]]
            [hive-ingest-kit.web-docs :as web-docs]
            [hive-ingestor-rfc.index :as index]
            [clojure.java.io :as io])
(:import [java.nio.file Files]
[java.nio.file FileSystems]
[java.nio.file Path]))

(def default-glob
  "Mirror files that are RFC documents. The mirror also carries indices,
   errata and per-series symlinks, which are not documents."
  "rfc*.txt")

(defn rfc-number
  "The RFC number a mirror file name denotes, or nil.

   Only bare rfcN.txt qualifies: the mirror also holds rfcN-errata.txt and
   similarly shaped files that are not the document itself."
  [path]
  (some-> (re-find #"(?i)/?rfc(\d+)\.txt$" (str path))
          second
          Long/parseLong))

(defn canonical-url
  "The rfc-editor URL an ingested mirror file is attributed to."
  [number]
  (str "https://www.rfc-editor.org/rfc/rfc" number ".txt"))

(defn window
  "The mirror entries selected by OPTS, in ascending RFC order.

   :from   — lowest RFC number, inclusive
   :to     — highest RFC number, inclusive
   :offset — entries to skip after filtering
   :limit  — maximum entries to return"
  [entries {:keys [from to offset limit]}]
  (cond->> (sort-by :rfc/number entries)
    from   (filter #(>= (:rfc/number %) from))
    to     (filter #(<= (:rfc/number %) to))
    true   vec
    offset (drop offset)
    limit  (take limit)
    true   vec))

(defn mirror-files
  "Absolute paths of the regular files under DIR whose DIR-relative path
   matches GLOB, sorted. Returns Result<vector<string>>; a missing or
   non-directory DIR is an error, not an empty mirror."
  [dir glob]
  (r/try-effect* :source/dir-expansion-failed
    (let [root (.toAbsolutePath (.toPath (io/file (str dir))))]
      (when-not (Files/isDirectory root (make-array java.nio.file.LinkOption 0))
        (throw (ex-info (str "Not a directory: " dir) {:dir dir})))
      (let [matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" glob))]
        (with-open [walk (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
          (->> (iterator-seq (.iterator walk))
               (filter #(Files/isRegularFile ^Path % (make-array java.nio.file.LinkOption 0)))
               (filter #(.matches matcher (.relativize root ^Path %)))
               (mapv str)
               sort
               vec))))))

(defn mirror-entries
  "Every RFC document in the mirror at DIR as {:path :rfc/number :url}.
   Returns Result<vector>."
  [dir {:keys [glob] :as opts}]
  (r/let-ok [paths (mirror-files dir (or glob default-glob))]
    (r/ok (window (into []
                        (keep (fn [path]
                                (when-let [n (rfc-number path)]
                                  {:path path :rfc/number n :url (canonical-url n)})))
                        paths)
                  opts))))

(defn- load-registry
  "The registry keyed by RFC number, or {} when the mirror has none.

   Read ONCE per run: a 10 MB index parsed per document would cost more than
   the ingest. An absent or unreadable index degrades to no status facets,
   never to a failed run — the documents are still worth having."
  [dir {:keys [index-path use-index?] :or {use-index? true}}]
  (if-not use-index?
    {}
    (let [path (or index-path (str (io/file dir index/index-file-name)))]
      (if (.isFile (io/file path))
        (let [parsed (index/parse path)]
          (if (r/ok? parsed) (:ok parsed) {}))
        {}))))

(defn- with-registry-facets
  "DOC carrying the registry's view of it: current status, and what obsoletes
   or updates it — knowledge the document's own masthead cannot contain."
  [doc registry]
  (let [number (some-> (get-in doc [:document/metadata :rfc/number]) str parse-long)]
    (if-let [record (get registry number)]
      (update doc :document/metadata index/enrich-metadata record)
      doc)))

(defrecord RfcMirrorSource [dir]
  ISource
  (source-id [_] "rfc")

  (fetch-documents [_ opts]
    (let [dir (or (:dir opts) dir)]
      (if (str/blank? (str dir))
        (r/err :source/invalid-config {:reason "no mirror directory specified"})
        (r/let-ok [entries (mirror-entries dir opts)]
          (let [registry (load-registry dir opts)]
            (r/ok (into []
                        (keep (fn [{:keys [path url]}]
                                (let [body (r/guard Exception nil (slurp path))
                                      doc  (when body
                                             (web-docs/body->document
                                              body
                                              {:url url :content-type "text/plain"}
                                              opts))]
                                  (when (and doc (r/ok? doc))
                                    (with-registry-facets (:ok doc) registry)))))
                        entries)))))))

  ISourceHealth
  (source-health [this]
    (let [entries (mirror-entries dir {})]
      (if (r/ok? entries)
        {:status :ok :details {:dir dir
                               :documents (count (:ok entries))
                               :registry? (.isFile (io/file (str (io/file dir index/index-file-name))))}}
        {:status :down :details {:dir dir :error (:error entries)}}))))

(defn rfc-mirror-source
  "Create an RfcMirrorSource over a local rsync mirror directory."
  [dir]
  (->RfcMirrorSource dir))
