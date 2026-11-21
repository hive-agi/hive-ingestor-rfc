(ns hive-ingestor-rfc.parser-test
  "The RFC parse profile, as a rule the addon REGISTERS ahead of the shared
   chain.

   Pinned:
   - a text/plain RFC (by URL or by masthead) is claimed by the addon's rule
   - text/plain that is NOT an RFC still goes to the generic plaintext rule
   - fetching an rfc-editor URL through the web source yields a spec document
     with masthead metadata while the addon is loaded"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.protocol :as proto]
            [hive-dsl.result :as r]
            [hive-html.page :as page]
            [hive-ingest-kit.parser-rules :as rules]
            [hive-ingest-kit.web-docs :as web]
            [hive-ingestor-rfc.addon :as addon]
            [hive-ingestor-rfc.parser :as parser]
            [hive-spi.ingest.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f]
                      (proto/initialize! (addon/addon-ctor) {})
                      (try (f) (finally (proto/shutdown! (addon/addon-ctor))))))

(def ^:private sample-rfc
  (slurp (io/resource "hive_ingestor_rfc/fixtures/sample-rfc.txt")))

(def ^:private rfc-body
  "Network Working Group                                          A. Author\nRequest for Comments: 9999                                  Example Corp\n\n                    Example Protocol\n")

(deftest the-registered-rule-claims-rfc-plaintext
  (doseq [ctx [{:url "https://www.rfc-editor.org/rfc/rfc2616.txt"
                :content-type "text/plain;charset=utf-8"
                :body "anything"}
               {:url "https://example.com/spec"
                :content-type "text/plain"
                :body rfc-body}]]
    (let [profile (rules/select-profile ctx)]
      (is (= parser/rule-id (:profile/id profile)) (pr-str ctx))
      (is (= :content/spec (:profile/content-kind profile)))
      (is (instance? hive_ingestor_rfc.parser.RfcTextParser (:profile/parser profile))))))

(deftest non-rfc-plaintext-stays-with-the-generic-rule
  (let [profile (rules/select-profile {:url "https://example.com/notes.txt"
                                       :content-type "text/plain"
                                       :body "just some notes"})]
    (is (= :parser-rule/plain-text (:profile/id profile)))
    (is (= :content/reference (:profile/content-kind profile)))))

(deftest html-is-never-claimed
  (is (not (ports/rule-applies? (parser/rfc-text-rule)
                                {:url "https://www.rfc-editor.org/rfc/rfc2616.html"
                                 :content-type "text/html"
                                 :body rfc-body}))))

(deftest the-parser-honours-the-port
  (let [parser (parser/rfc-text-parser)]
    (is (satisfies? page/IWebsiteParser parser))
    (is (r/err? (page/parse-website-page parser "" {:url "u"})))
    (let [page (:ok (page/parse-website-page parser sample-rfc {:url "u"}))]
      (is (= "9999" (:rfc/number page)))
      (is (= "Example Protocol for Testing" (:title page)))
      (is (= "https://www.rfc-editor.org/info/rfc9999" (:canonical-url page)))
      (is (= [{:href "https://www.rfc-editor.org/rfc/rfc9998.txt" :text "RFC 9998"}]
             (:supersedes page)))
      (is (not (str/includes? (:content page) "[Page 1]"))))))

(deftest web-ingestion-of-an-rfc-editor-url-parses-as-a-spec
  (let [http-get (fn [_url _opts]
                   {:status 200 :headers {"content-type" "text/plain"} :body sample-rfc})
        result   (ports/fetch-documents
                  (web/web-doc-source ["https://www.rfc-editor.org/rfc/rfc9999.txt"])
                  {:http-get-fn http-get})
        doc      (first (:ok result))
        md       (:document/metadata doc)]
    (is (r/ok? result))
    (is (= :content/spec (:content-kind md)))
    (is (= "9999" (:rfc/number md)))
    (is (= "Example Protocol for Testing" (:title md)))))
