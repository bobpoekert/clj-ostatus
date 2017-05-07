(ns ostatus.html
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element])
  (:require [ostatus.util :as u]
            [clojurewerkz.urly.core :as uu]
            [clojure.string :as s]))

(set! *warn-on-reflection* true)

(defn tag-attr
  [^Document tree ^String query ^String attr]
  (if-let [tag (.select tree query)]
    (if-let [tag (.first tag)]
      (.attr tag attr))))

(defn tag-text
  [^Document tree ^String query]
  (if-let [tag (.select tree query)]
    (if-let [tag (.first tag)]
      (.text tag))))

(defprotocol ParseHtml
  (parse-html [v url]))

(extend-protocol ParseHtml
  String
  (parse-html [v ^String url] (Jsoup/parse v url))
  java.io.InputStream
  (parse-html [v ^String url] (Jsoup/parse v "UTF-8" url)))

(defn extract-account
  [html-blob base-uri]
  (let [tree (parse-html html-blob base-uri)
        ta (partial tag-attr tree)]
    {
      :foaf-url (ta "link[rel='meta'][type='application/rdf+xml']" "href")
      :atom-url (ta "link[rel='alternate'][type='application/atom+xml']" "href")
      :rss-url (ta "link[rel='alternate'][type='application/rss+xml']" "href")
      :activitystream-url (ta "link[rel='alternate'][type='application/stream+json']" "href")
      :bio (or 
            (ta "meta[name='description']" "content")
            (ta "meta[name='og:description']" "content"))
      :av (ta "meta[property='og:image']" "content")}))
  
(defn extract-mastodon-followers
  [follower-page follower-page-url]
  (let [^Document tree (parse-html follower-page follower-page-url)
        url (partial uu/resolve follower-page-url)]
    {:followers
      (for [item (.select tree ".account-grid-card")]
        (let [ta (partial tag-attr item)
              tx (partial tag-text item)
              username (tx ".username")
              username (if (s/starts-with? username "@") (subs username 1) username)
              username-key (if (s/includes? username "@") :qualified-username :username)
              av (ta ".avatar img" "src")]
          {
            username-key username
            :av (if (s/includes? av "missing.png") nil (url av))
            :html-url (url (ta ".name a" "href"))
            :bio (tx ".note")
            :display-name (tx ".display_name")}))
    :page-count (->>
                  (.select tree ".page")
                  (map (fn [^Element t] (u/number (.text t))))
                  (filter identity)
                  (apply max))}))
