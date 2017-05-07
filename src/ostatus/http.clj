(ns ostatus.http
  (:require [clojure.java.io :as io]
            [clojurewerkz.urly.core :as uu]
            [aleph.http :as h]
            [manifold.deferred :as d]
            [manifold.time :as mt]
            [manifail :as ff] 
            [byte-streams :as bs]
            [clojure.string :as s]
            [ostatus.types :as c]
            [ostatus.webfinger :as wf]
            [ostatus.atom :as a]
            [ostatus.html :as html]))


(defn- default-req
  ([method url]
    (default-req method url nil))
  ([method url options]
    (h/request
      (assoc options
        :request-method method
        :throw-exceptions false
        :url url))))

(def ^{:dynamic true
       :doc "http request function; can be stubbed out for testing"} req default-req)

(defn success?
  [response]
  (= (:status response) 200))

(defn retry-req
  [& args]
  (->
    (ff/with-retries (ff/delay (ff/retries 5) 100 {:backoff-factor 2.0 :jitter-factor 0.5})
      (d/let-flow [response (apply req args)]
        (if (success? response)
          response
          (ff/retry! response))))
    (d/catch manifail.RetriesExceeded ff/unwrap)))

(defn get-string
  [url]
  (d/let-flow [res (req :get url)]
    (if (success? res)
      (bs/to-string (:body res)))))

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

(defn get-feed-from-url
  ([atom-url]
    (get-feed-from-url {} atom-url))
  ([account atom-url]
    (d/let-flow [feed-response (req :get atom-url)]
      (if (success? feed-response)
        (let [res (a/parse (bs/to-string (:body feed-response)))
              account (rec-merge account (:account res))]
          (c/map-> Feed {
            :account account
            :posts (for [post (:posts res)]
                    (if (= (:username (:author post)) (:username account))
                      (assoc post :author account)
                      post))}))))))

(defn get-feed
  [qualified-username]
  (d/let-flow [account (get-account qualified-username)]
    (get-feed-from-url account (:atom-url account))))

(defn get-account-from-url
  [url]
  (let [webfinger-url (format "https://%s/.well-known/webfinger?resource=%s" (uu/host-of url) url)]
    (d/let-flow [webfinger-response (req :get webfinger-url)]
      (if (success? webfinger-response)
        (wf/decode-webfinger-json (bs/to-string (:body webfinger-response)))
        (d/let-flow [page-response (req :get url)]
          (if (success? page-response)
            (html/extract-account (:body page-response) url)))))))

(defn d-zip-sequential
  ([s] (d-zip-sequential s 5))
  ([s max-concurrent]
    (d/loop [res []
             s s]
      (let [head (take max-concurrent s)
            tail (drop max-concurrent s)]
        (d/let-flow [slice (apply d/zip head)]
          (let [res (into res slice)]
            (if (seq tail)
              (d/recur res tail)
              res)))))))

(defn dedupe-followers
  [followers]
  (second
    (reduce
      (fn [[url-set res] follower]
        (let [url (:html-url follower)]
          (if (contains? url-set url)
            [url-set res]
            [(conj url-set url) (conj res follower)])))
      [#{} []]
      followers)))

(defn get-mastodon-followers
  ([username host] (get-mastodon-followers username host "followers"))
  ([username host verb]
    (let [url (format "https://%s/users/%s/%s" host username verb)]
      (d/let-flow [first-page (req :get url)]
        (if (success? first-page)
          (let [parsed (html/extract-mastodon-followers (:body first-page) url)
                page-requests (for [page-id (range 2 (:page-count parsed))]
                                (retry-req :get (format "%s?page=%d" url page-id)))]
            (d/let-flow [responses (d-zip-sequential page-requests)]
              (dedupe-followers
                (reduce concat (:followers parsed)
                  (for [response (filter success? responses)]
                    (let [parsed (html/extract-mastodon-followers (:body response) url)]
                      (:followers parsed))))))))))))

(defn get-mastodon-following
  [username host]
  (get-mastodon-followers username host "following"))

(defn get-mastodon-friends
  [username host]
  (d/let-flow [followers (get-mastodon-followers username host)
               following (get-mastodon-following username host)]
    {:followers followers
     :following following}))
