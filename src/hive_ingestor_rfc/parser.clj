(ns hive-ingestor-rfc.parser
  "The RFC parse profile: RfcTextParser and the IParserRule that selects it.

   Registered through hive-spi.ingest.registry on addon initialize!, so a
   text/plain RFC is claimed BEFORE the generic plaintext rule of the shared
   chain; any other text/plain body still falls through to that rule."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-html.page :refer [IWebsiteParser]]
            [hive-ingestor-rfc.structure :as rfc]
            [hive-spi.ingest.model :as model]
            [hive-spi.ingest.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Parser
;; =============================================================================

(defrecord RfcTextParser []
  IWebsiteParser
  (parse-website-page [_ body {:keys [url]}]
    (if (str/blank? body)
      (r/err :source/empty-body {:url url})
      (r/try-effect* :source/parse-failed
                     (let [content  (rfc/depaginate body)
                           lines    (str/split-lines content)
                           masthead (or (first (rfc/preamble-blocks lines)) [])
                           front    (merge (rfc/masthead-fields masthead)
                                           (rfc/masthead-authors masthead))
                           number   (:rfc/number front)]
                       (merge front
                              {:url           url
                               :canonical-url (when number
                                                (str "https://www.rfc-editor.org/info/rfc" number))
                               :title         (or (rfc/title lines) url)
                               :description   (rfc/abstract lines)
                               :content       content
                               :headings      (rfc/headings content)
                               :links         (rfc/references content number)
                               :supersedes    (rfc/superseded (:rfc/obsoletes front))}))))))

(defn rfc-text-parser [] (->RfcTextParser))

;; =============================================================================
;; Rule
;; =============================================================================

(defn- mime-type
  "The bare MIME type of a Content-Type header, lowercased."
  [content-type]
  (some-> content-type str/lower-case (str/split #";") first str/trim))

(defn plain-text?
  "True when the response is text/plain, or, absent a Content-Type, the URL
   carries a plaintext suffix."
  [{:keys [url content-type]}]
  (if-let [mime (mime-type content-type)]
    (= "text/plain" mime)
    (boolean (re-find #"(?i)\.(txt|text)(\?.*)?$" (str url)))))

(defn rfc-document?
  "True when the response body or URL identifies an IETF RFC."
  [{:keys [url body]}]
  (let [body (str body)]
    (boolean
     (or (re-find #"(?i)rfc-?editor\.org/rfc/rfc\d+" (str url))
         (re-find #"(?im)^Request for Comments:\s*\d+"
                  (subs body 0 (min 4000 (count body))))))))

(def rule-id :parser-rule/rfc-text)

(defrecord RfcTextRule []
  ports/IParserRule
  (rule-id [_] rule-id)
  (rule-applies? [_ ctx] (and (plain-text? ctx) (rfc-document? ctx)))
  (rule-profile [_ _]
    {:profile/id           rule-id
     :profile/format       (model/document-format :format/text)
     :profile/content-kind :content/spec
     :profile/parser       (rfc-text-parser)}))

(defn rfc-text-rule [] (->RfcTextRule))
