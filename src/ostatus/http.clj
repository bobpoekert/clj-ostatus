(ns ostatus.http
  (:import [java.net URL])
  (:require [clojure.java.io :as io]
            [aleph.http :as h]
            [manifold.deferred :as d]
            [byte-streams :as bs]
            [clojure.string :as s]
            [ostatus.types :as c]
            [ostatus.webfinger :as wf]
            [ostatus.atom :as a]))

(defn host
  [url]
  (.getHost (URL. url)))

(defn- default-req
  ([method url]
    (default-req method url nil))
  ([method url options]
    (h/request
      (assoc options
        :request-method method
        :url url))))

(def ^{:dynamic true
       :doc "http request function; can be stubbed out for testing"} req default-req)

(defn success?
  [response]
  (= (:status response) 200))

(defn rec-merge
  [a b]
  (reduce
    (fn [res [k v]]
      (if (nil? v)
        res
        (assoc res k v)))
  a b))

(defn get-account
  [qualified-username]
  (let [url (wf/webfinger-url qualified-username)]
    (d/let-flow [response (req :get url)]
      (if (success? response) 
        (wf/decode-webfinger-json (bs/to-string (:body response)))))))

(defn get-feed
  [qualified-username]
  (d/let-flow [account (get-account qualified-username)
               feed-response (req :get (:atom-url account))]
    (if (success? feed-response)
      (let [res (a/parse (bs/to-string (:body feed-response)))
            account (rec-merge account (:account res))]
        (c/map-> Feed {
          :account account
          :posts (for [post (:posts res)]
                  (if (= (:username (:author post)) (:username account))
                    (assoc post :author account)
                    post))})))))

(defn get-account-from-url
  [url]
  (let [webfinger-url (format "https://%s/.well-known/webfinger?resource=%s" (host url) url)]
    (d/let-flow [webfinger-response (req :get webfinger-url)]
      (if (success? webfinger-response)
        (wf/decode-webfinger-json (bs/to-string (:body webfinger-response)))))))
