(ns hive-ingestor-rfc.tck-test
  "The RFC mirror source proven against the hive-spi.ingest conformance kit.

   A mirror is a directory, so the fixture is a temp directory holding a few
   rfcN.txt files plus the residents a real mirror carries (errata, index)
   that must not count as documents. Nothing touches rsync or the network."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-ingestor-rfc.mirror :as mirror]
            [hive-spi.ingest.tck :as tck])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- rfc-text
  "A minimal RFC body numbered N, shaped like the RFC series header."
  [n]
  (str "Network Working Group                                     A. Author\n"
       "Request for Comments: " n "                                  Example\n"
       "Category: Standards Track                             February 1997\n"
       "\n\n                   Example Protocol " n "\n\n"
       "Status of this Memo\n\n   This document specifies a protocol.\n\n"
       "Abstract\n\n   This document specifies example " n ".\n\n"
       "1. Terminology\n\n   The terms apply as usual.\n\n"
       "2. Overview\n\n   An origin server may send state information.\n"))

(defn- mirror-dir
  "A temp mirror directory holding FILES (name -> content)."
  [files]
  (let [root (.toFile (Files/createTempDirectory "rfc-tck-mirror"
                                                 (into-array FileAttribute [])))]
    (.deleteOnExit root)
    (doseq [[name content] files]
      (let [f (java.io.File. root ^String name)]
        (spit f content)
        (.deleteOnExit f)))
    (str root)))

(def ^:private fixtures
  {:opts       {}
   :limit-opts {:limit 2}})

(deftest rfc-mirror-conforms-to-the-ingest-tck
  (let [dir    (mirror-dir (merge (into {} (map (fn [n] [(str "rfc" n ".txt") (rfc-text n)]))
                                    [2109 7230 9110])
                                  {"rfc2109-errata.txt" "errata"
                                   "rfc-index.txt"      "index"}))
        src    (mirror/rfc-mirror-source dir)
        report (tck/conform src fixtures)]
    (is (:ok report) (tck/explain src fixtures))
    (testing "measured at BOTH rungs, with nothing skipped"
      (is (= #{:rung/descriptor :rung/behaviour} (:rungs-measured report)))
      (is (empty? (:skips report)) (pr-str (:skips report))))))
