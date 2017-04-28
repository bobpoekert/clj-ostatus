(ns ostatus.util
  (:import [java.time Instant]
           [org.jsoup Jsoup])
  (:require [clj-xpath.core :refer :all]))

(defn to-iso-string
  [epoch-int]
  (if-not (nil? epoch-int)
    (.toString (Instant/ofEpochSecond epoch-int))))

(defn from-iso-string
  [^String v]
  (if v
    (.getEpochSecond (Instant/parse v))))

(defn strip-html
  [^String s]
  (if s
    (.text (Jsoup/parse s))))

(def namespaces {
  :atom        "http://www.w3.org/2005/Atom"
  :media  "http://purl.org/syndication/atommedia"
  :activity     "http://activitystrea.ms/spec/1.0/"
  :thr    "http://purl.org/syndication/thread/1.0"
  :poco   "http://portablecontacts.net/spec/1.0"
  :dfrn   "http://purl.org/macgirvin/dfrn/1.0"
  :os     "http://ostatus.org/schema/1.0"
  :mtdn  "http://mastodon.social/schema/1.0"
  :magic "http://salmon-protocol.org/ns/magic-env"})

(def verbs {
  :post           "http://activitystrea.ms/schema/1.0/post"
  :share          "http://activitystrea.ms/schema/1.0/share"
  :favorite       "http://activitystrea.ms/schema/1.0/favorite"
  :unfavorite     "http://activitystrea.ms/schema/1.0/unfavorite"
  :delete         "http://activitystrea.ms/schema/1.0/delete"
  :follow         "http://activitystrea.ms/schema/1.0/follow"
  :request_friend "http://activitystrea.ms/schema/1.0/request-friend"
  :authorize      "http://activitystrea.ms/schema/1.0/authorize"
  :reject         "http://activitystrea.ms/schema/1.0/reject"
  :unfollow       "http://ostatus.org/schema/1.0/unfollow"
  :block          "http://mastodon.social/schema/1.0/block"
  :unblock        "http://mastodon.social/schema/1.0/unblock"})

(def types {
  :activity   "http://activitystrea.ms/schema/1.0/activity"
  :note       "http://activitystrea.ms/schema/1.0/note"
  :comment    "http://activitystrea.ms/schema/1.0/comment"
  :person     "http://activitystrea.ms/schema/1.0/person"
  :collection "http://activitystrea.ms/schema/1.0/collection"
  :group "http://activitystrea.ms/schema/1.0/group"})

(def collections {
  :public "http://activityschema.org/collection/public"})

(defn number
  [v]
  (if v (Integer/decode v)))

(defn normalize-url
  [^String url]
  (let [url (.trim url)]
    (if (.endsWith url "/")
      url
      (str url "/"))))

(defn ns-context
  [namespaces]
  (let [namespaces (zipmap (map name (keys namespaces)) (vals namespaces))
        inverse (zipmap (vals namespaces) (keys namespaces))]
    (proxy [javax.xml.namespace.NamespaceContext] []
      (getNamespaceURI
        [prefix]
        (get namespaces prefix javax.xml.XMLConstants/NULL_NS_URI))
      (getPrefixes
        [v]
        nil)
      (getPrefix
        [uri]
        (get inverse uri)))))

(def masto-xpath-compiler (.newXPath *xpath-factory*))
(.setNamespaceContext masto-xpath-compiler
  (ns-context namespaces))

(defmacro with-masto-ns
  [& bodies]
  `(binding [clj-xpath.core/*namespace-aware* true
             clj-xpath.core/*xpath-compiler* masto-xpath-compiler]
    ~@bodies))

(defn read-doc
  [doc]
  (xml->doc doc {:namespace-aware true}))

(defn attr-getter
  [node]
  (fn [q attr]
    (if-let [t ($x:attrs? q node)]
      (get t attr))))

(defn text-getter
  [node]
  (fn [q]
    ($x:text? q node)))