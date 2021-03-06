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
            [ostatus.foaf :as foaf]
            [ostatus.html :as html]))

(def connection-stats (atom nil))

(def connection-pool
  (h/connection-pool
    {:connections-per-host 20
     :total-connections 65536
     :stats-callback (fn [v] (swap! connection-stats (fn [a] v)))}))

(defn- default-req
  ([method url]
    (default-req method url nil))
  ([method url options]
    (h/request
      (assoc options
        :pool connection-pool
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
    (ff/with-retries (ff/delay (ff/retries 10) 100 {:backoff-factor 2.0 :jitter-factor 0.5})
      (d/let-flow [response (apply req args)]
        (if (success? response)
          response
          (do (prn "retry") (ff/retry! response)))))
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

(defn mastodon? 
  [response]
  (= (get (:headers response) "server") "Mastodon"))

(defn get-account-from-username
  [qualified-username]
  (let [url (wf/webfinger-url qualified-username)]
    (d/let-flow [response (req :get url)]
      (if (success? response) 
        (assoc
          (wf/decode-webfinger-json (bs/to-string (:body response)))
          :mastodon? (mastodon? response))))))

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

(defn get-post-from-url
  [post-url]
  (d/let-flow [response (req :get post-url)]
    (if (success? response)
      (let [t (get (:headers response) "Content-Type")]
        (cond
          (s/includes? t "text/html")  (let [info (html/extract-account (:body response) post-url)]
                                          (if (:atom-url info) (get-post-from-url (:atom-url info))))
          (s/includes? t "application/atom+xml") (first (a/parse (bs/to-string (:body response)))))))))

(defn get-account-from-url
  [url]
  (let [webfinger-url (format "https://%s/.well-known/webfinger?resource=%s" (uu/host-of url) url)]
    (d/let-flow [webfinger-response (req :get webfinger-url)]
      (if (success? webfinger-response)
        (assoc
          (wf/decode-webfinger-json (bs/to-string (:body webfinger-response)))
          :mastodon? (mastodon? webfinger-response))
        (d/let-flow [page-response (req :get url)]
          (if (success? page-response)
            (assoc
              (html/extract-account (:body page-response) url)
              :mastodon? false)))))))

(defn get-account
  [account]
  (d/let-flow [res (cond
              (:qualified-username account) (get-account-from-username (:qualified-username account))
              (and (:username account) (:host account)) (get-account (format "%s@%s" (:qualified-username account) (:host account)))
              (:html-url account) (get-account-from-url (:html-url account))
              :else {})]
    (merge account res)))

(defn get-feed
  [account]
  (if (:atom-url account)
    (get-feed-from-url (:atom-url account))
    (d/let-flow [account (get-account account)]
      (get-feed account))))

(defn d-pmap
  "like pmap, but takes a seq of defereds and returns a defered"
  ([s] (d-pmap identity s))
  ([f s] (d-pmap f s 5))
  ([f s max-concurrent]
    (let [state (atom [[0 s] [0 0 {}]])
          res (d/deferred)
          got-result (fn got-result [pop-next idx result]
                      (when-not (contains? (last (last @state)) idx)
                        (swap! state
                          (fn [[inp [started-count finished-count result-values]]]
                            [inp [started-count (inc finished-count) (assoc result-values idx (f result))]]))
                        (pop-next)))
          pop-next (fn pop-next []
                    (swap! state
                      (fn [[[idx s] [start-count finished-count result]]]
                        (if (seq s)
                          (do 
                            (d/chain (first s) (partial got-result pop-next idx))
                            [[(inc idx) (rest s)] [(inc start-count) finished-count result]])
                          (do
                            (if (>= finished-count start-count) 
                              (d/success! res (for [i (range finished-count)] (get result i))))
                            [[idx s] [start-count finished-count result]])))))]
      (dotimes [i max-concurrent]
        (pop-next))
      res)))

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
      (d/let-flow [first-page (retry-req :get url)]
        (if (success? first-page)
          (let [parsed (html/extract-mastodon-followers (:body first-page) url)
                page-requests (for [page-id (range 2 (:page-count parsed))]
                                (retry-req :get (format "%s?page=%d" url page-id)))]
            (d/let-flow [responses (d-pmap #(if (success? %)
                                              (html/extract-mastodon-followers (:body %) url))
                                            page-requests)]
              (assoc parsed :followers 
                (dedupe-followers
                  (reduce concat (:followers parsed)
                    (map :followers (filter identity responses))))))))))))

(defn get-mastodon-following
  [username host]
  (get-mastodon-followers username host "following"))

(defn get-mastodon-friends
  [username host]
  (d/let-flow [followers (get-mastodon-followers username host)
               following (get-mastodon-following username host)]
    (->
      (merge followers following)
      (assoc :followers (:followers followers))
      (assoc :following (:followers following)))))

(defn get-friends
  [account-info]
  (cond
    (:foaf-url account-info) (d/chain (retry-req :get (:foaf-url account-info)) #(if (success? %) (foaf/parse-foaf (:body %))))
    (:mastodon? account-info) (get-mastodon-friends (:username account-info) (uu/host-of (:html-url account-info)))
    (not (:getting-friends? account-info)) (d/let-flow [acc (get-account account-info)] (get-friends (assoc acc :getting-friends? true)))
    :else (delay {:followers [] :following []})))

