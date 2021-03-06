(ns ostatus.util
  (:import [java.time Instant]
           [org.jsoup Jsoup]
           [org.w3c.dom Document Element Node]
           [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           [javax.xml.transform Transformer TransformerFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.xpath XPath]
           [java.io StringWriter]
           ThreadLocalThing)
  (:require [clj-xpath.core :refer :all]
            [clojure.string :as s]))

(defn all?
  [s]
  (loop [[h & t] s]
    (if h
      (if t
        (recur t)
        true)
      false)))

(defn make-thread-local
  [generator]
  (ThreadLocalThing. ::initial-val generator))

(defmacro thread-local
  [& body]
  `(make-thread-local (fn [] ~@body)))

(set! *warn-on-reflection* true)

(defn hash-string
  [v]
  (if (nil? v)
    ""
    (format "%x" (clojure.lang.Murmur3/hashInt (.hashCode ^Object v)))))

(defn to-iso-string
  [epoch-int]
  (cond
    (nil? epoch-int) nil
    :else (.toString (Instant/ofEpochSecond epoch-int))))

(defn from-iso-string
  [^String v]
  (if-not (nil? v)
    (let [v (.replace v "+00:00" "Z")]
      (.getEpochSecond (Instant/parse v)))))

(defn strip-html
  [^String s]
  (if-not (nil? s)
    (.text (Jsoup/parse s))))

(defn escape-html
  [^String s]
  (.. s
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")
    (replace "'" "&#39;")))

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
  (if v 
    (let [v (s/replace v #"[^0-9\.\-e]" "")]
      (try
        (Long/decode v)
        (catch NumberFormatException e
          nil)))))

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

(defn maybe-set-prop!
  [k v]
  (if (nil? (System/getProperty k))
    (System/setProperty k v)))

;; workaround for http://stackoverflow.com/questions/6340802/java-xpath-apache-jaxp-implementation-performance
(maybe-set-prop! "org.apache.xml.dtm.DTMManager" "org.apache.xml.dtm.ref.DTMManagerDefault")
(maybe-set-prop! "com.sun.org.apache.xml.internal.dtm.DTMManager" "com.sun.org.apache.xml.internal.dtm.ref.DTMManagerDefault")

(defrecord XpathContext [
  ^javax.xml.xpath.XPathFactory xpath-factory
  ^XPath xpath-compiler
  ^java.util.Map compiled-xpaths])

(defn xpath-context
  [namespaces]
  (thread-local
    (let [factory (org.apache.xpath.jaxp.XPathFactoryImpl.)
          ^XPath compiler (.newXPath factory)]
      (.setNamespaceContext compiler
        (ns-context namespaces))
      (->XpathContext factory compiler (java.util.HashMap.)))))

(def masto-context (xpath-context namespaces))

(def ^{:dynamic true} *current-xpath-context* nil)
(defmacro with-xpath-ns
  [context & bodies]
  `(binding [clj-xpath.core/*namespace-aware* true
             *current-xpath-context* (deref ~context)
             clj-xpath.core/*xpath-compiler* (:xpath-compiler *current-xpath-context*)]
    ~@bodies))

(def ^{:dynamic true} *is-masto-ns* false)
(defmacro with-masto-ns
  [& bodies]
  `(binding [*is-masto-ns* true]
    (with-xpath-ns masto-context ~@bodies)))

(defn xp
  [^String query]
  (if *current-xpath-context*
    (let [ctx *current-xpath-context*
          res (get (:compiled-xpaths ctx) query)]
      (if res
        res
        (let [res (.compile ^XPath (:xpath-compiler ctx) query)]
          (.put ^java.util.Map (:compiled-xpaths ctx) query res)
          res)))
    query))

(defn read-doc
  [doc]
  (xml->doc doc {:namespace-aware true :disallow-doctype-decl false}))

(defn attr-getter
  [node]
  (fn [q attr]
    (let [q (xp q)]
      (if-let [t ($x:attrs? q node)]
        (get t attr)))))

(defn text-getter
  [node]
  (fn [q]
    (let [q (xp q)
          ^String res ($x:text? q node)]
      (if (and res (not= (.indexOf res "<") -1))
        (strip-html res)
        res))))

(defn apply-attributes!
  [^Element tag attrs]
  (doseq [[k v] attrs]
    (if-not (nil? v)
      (.setAttribute tag (name k) (str v)))))

(defprotocol MakeDomTag
  (make-dom-tag [v ^Document doc]))

(extend-protocol MakeDomTag
  clojure.lang.PersistentVector
  (make-dom-tag [v ^Document doc]
    (let [tag (.createElement doc (name (first v)))]
      (if (map? (second v))
        (do
          (apply-attributes! tag (second v))
          (doseq [child (rest (rest v))]
            (.appendChild tag (make-dom-tag child doc))))
        (doseq [child (rest v)]
          (.appendChild tag (make-dom-tag child doc))))
      tag))
  String
  (make-dom-tag [v ^Document doc]
    (.createTextNode doc v))
  clojure.lang.Seqable
  (make-dom-tag [v ^Document doc]
    (let [f (.createDocumentFragment doc)]
      (doseq [e v]
        (.appendChild f (make-dom-tag e doc)))
      f))
  Object
  (make-dom-tag [v ^Document doc]
    (make-dom-tag (str v) doc))
  nil
  (make-dom-tag [v ^Document doc]
    (make-dom-tag "" doc)))
   
(def ^DocumentBuilder doc-factory (.newDocumentBuilder (DocumentBuilderFactory/newInstance)))
(defn ^Document new-doc
  []
  (.newDocument doc-factory))

(def ^TransformerFactory transformer-factory (TransformerFactory/newInstance))
(defn render-document
  [^Document doc]
  (let [^Transformer transformer (.newTransformer transformer-factory)
        ^DOMSource source (DOMSource. doc)
        ^StringWriter outp (StringWriter.)
        ^StreamResult sr (StreamResult. outp)]
    (.transform transformer source sr)
    (.toString outp)))

(defn render-xml
  [tree]
  (let [doc (new-doc)]
    (.appendChild doc (make-dom-tag tree doc))
    (render-document doc)))

(def xmlns-attrs
  (zipmap
    (map #(keyword (format "xmlns:%s" (name %))) (keys namespaces))
    (vals namespaces)))

(defmacro xmlns-tag
  [tagname root-ns & children]
  (into [tagname (assoc xmlns-attrs :xmlns (get namespaces root-ns))] children))
