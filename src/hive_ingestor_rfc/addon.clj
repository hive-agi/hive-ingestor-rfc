(ns hive-ingestor-rfc.addon
  "IAddon that teaches hive-ingestor about the RFC series.

   The host owns the seams; this addon owns the corpus. On initialize! it
   registers its ISource factory in the host's source registry, so the series
   becomes reachable as `ingest source source=rfc dir=...` without the host
   ever naming an RFC. shutdown! retracts every registration it owns."
  (:require [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]
            [hive-ingestor-rfc.mirror :as mirror]
            [hive-spi.ingest.registry :as source-registry]
            [taoensso.timbre :as log]))

(def addon-id "hive.ingestor.rfc")

(def source-id
  "The id the corpus is driven by, and — via the host's corpus scoping — the
   scope it lands in (topic:rfc)."
  "rfc")

(def source-registration
  {:factory     (fn [{:keys [dir]}] (mirror/rfc-mirror-source dir))
   :description (str "The IETF RFC series from a local rsync mirror "
                     "(rsync -avz --delete rsync.rfc-editor.org::rfcs-text-only <dir>). "
                     "A run is a window over ~10k documents; re-running a window upserts.")
   :params      {"dir"    "Local rsync mirror directory (required)"
                 "from"   "Lowest RFC number to ingest, inclusive"
                 "to"     "Highest RFC number to ingest, inclusive"
                 "offset" "Documents to skip after filtering"
                 "limit"  "Maximum documents in this run"
                 "glob"   "Mirror file glob (default: rfc*.txt)"}})

(defonce ^:private addon-state (atom nil))

(defn register!
  "Register this addon's corpus with the host. Returns Result."
  []
  (source-registry/register-source! addon-id source-id source-registration))

(defn retract!
  "Remove every registration this addon owns. Returns Result."
  []
  (source-registry/retract-all! addon-id))

(defrecord RfcCorpusAddon [id])

(extend-type RfcCorpusAddon
  proto/IAddon

  (addon-id [{:keys [id]}] (or id addon-id))

  (addon-type [_] :native)

  (capabilities [_] #{:sources})

  (initialize! [_ _config]
    ;; Registration is owner-idempotent, so it is re-asserted on every call
    ;; rather than skipped: an addon that reports success while the host has
    ;; no record of its corpus is the failure worth preventing here.
    (let [already? (some? @addon-state)
          result   (register!)]
      (if (r/ok? result)
        (do (reset! addon-state {:initialized-at (java.time.Instant/now)})
            (log/info "hive-ingestor-rfc registered its corpus" {:source source-id})
            (cond-> {:success? true :errors [] :metadata {:source source-id}}
              already? (assoc :already-initialized? true)))
        (do (log/error "hive-ingestor-rfc could not register its corpus"
                       {:error (:error result)})
            {:success? false :errors [(str (:error result))]}))))

  (shutdown! [_]
    (retract!)
    (reset! addon-state nil)
    (log/info "hive-ingestor-rfc retracted its registrations")
    nil)

  (tools [_] [])

  (schema-extensions [_] {})

  (health [_]
    (if @addon-state
      {:status :ok :details {:source source-id}}
      {:status :degraded :details {:reason "not initialized"}})))

(defn addon-ctor
  "Entry point named by the addon manifest."
  ([] (->RfcCorpusAddon addon-id))
  ([_config] (->RfcCorpusAddon addon-id)))
