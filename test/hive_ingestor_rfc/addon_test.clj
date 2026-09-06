(ns hive-ingestor-rfc.addon-test
  "Registration through the host's seam, not into the host's source."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.protocol :as proto]
            [hive-ingestor-rfc.addon :as addon]
            [hive-ingestor-rfc.mirror :as mirror]
            [hive-spi.ingest.ports :as sp]
            [hive-spi.ingest.registry :as registry]))

(use-fixtures :each (fn [f] (proto/shutdown! (addon/addon-ctor)) (f) (proto/shutdown! (addon/addon-ctor))))

(deftest the-corpus-is-not-known-until-the-addon-registers-it
  (testing "the host ships no knowledge of the RFC series"
    (is (nil? (registry/source-factory addon/source-id))))
  (testing "initialize! is what teaches it"
    (let [result (proto/initialize! (addon/addon-ctor) {})]
      (is (:success? result))
      (is (fn? (registry/source-factory addon/source-id)))))
  (testing "and shutdown! forgets it again"
    (proto/shutdown! (addon/addon-ctor))
    (is (nil? (registry/source-factory addon/source-id)))))

(deftest the-registered-factory-builds-the-mirror-source
  (proto/initialize! (addon/addon-ctor) {})
  (let [factory (registry/source-factory addon/source-id)
        source  (factory {:dir "/mirror"})]
    (is (instance? hive_ingestor_rfc.mirror.RfcMirrorSource source))
    (testing "and the source names itself, so the corpus gets its own scope"
      (is (= "rfc" (sp/source-id source))))))

(deftest initialize-is-idempotent
  (let [a (addon/addon-ctor)]
    (is (:success? (proto/initialize! a {})))
    (is (:already-initialized? (proto/initialize! a {})))))

(deftest the-registration-describes-itself-for-the-tool-surface
  (proto/initialize! (addon/addon-ctor) {})
  (let [entry (first (filter #(= "rfc" (:source-id %)) (registry/registered-sources)))]
    (is (= addon/addon-id (:owner entry)))
    (is (re-find #"rsync" (:description entry)))
    (testing "the window params a caller needs are advertised"
      (is (every? (set (keys (:params entry))) ["dir" "from" "to" "offset" "limit"])))))

(deftest another-owner-cannot-hijack-the-registration
  (proto/initialize! (addon/addon-ctor) {})
  (let [result (registry/register-source! "someone.else" addon/source-id
                                          {:factory (fn [_] nil)})]
    (is (= :source-registry/owner-conflict (:error result)))
    (testing "and the real factory still stands"
      (is (fn? (registry/source-factory addon/source-id))))))
