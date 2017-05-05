(ns ostatus.atom
  (:require [clj-xpath.core :refer :all]
            [ostatus.types :as c]
            [ostatus.util :refer :all]))

(set! *warn-on-reflection* true)

(defn parse-author
  [author]
  (with-masto-ns
    (let [av (first ($x:attrs* (xp "./atom:link[@rel='avatar']") author))
          header (first ($x:attrs* (xp "./atom:link[@rel='header']") author))
          text (text-getter author)
          attr (attr-getter author)]
      (c/map-> Account {
        :username (text "./atom:name")
        :qualified-username (text "./atom:email")
        :bio (text "./atom:summary")
        :html-url (or
                    (attr "./atom:link[@rel='alternate' and @type='text/html']" :href)
                    (text "./atom:uri"))
        :hub-url (attr "./atom:link[@rel='hub']" :href)
        :av (if av (:href av))
        :av-width (if av (number (:media:width av)))
        :av-height (if av (number (:media:height av)))
        :av-type (if av (:type av))
        :header-image (if header (:href header))
        :header-image-width (if header (number (:media:width header)))
        :header-image-height (if header (number (:media:height header)))
        :header-image-type (if header (:type header))
        :display-name (text "./poco:displayName")
        :scope (text "./mtdn:scope")}))))

(defn get-objects
  [node rel typ]
  (with-masto-ns
    ($x:attrs*
      (xp
        (format "./atom:link[@rel='%s' and @os:object-type='%s']"
          rel (get types typ)))
      node)))

(defn map->ref
  [m]
  (if m
    (if (:ref m)
      (c/->AtomRef (:ref m) (:href m))
      (:href m))))

(defn parse-attachment-tag
  [attachment]
  (c/map-> Attachment {
    :href (:href attachment)
    :type (:type attachment)
    :length (number (:length attachment))}))

(defn parse-entry
  ([entry] (parse-entry entry nil))
  ([entry author]
    (with-masto-ns
      (let [text (text-getter entry)
            attr (attr-getter entry)]
        (c/map-> Post {
          :published (from-iso-string (text "./atom:published"))
          :updated (from-iso-string (text "./atom:updated"))
          :title (text "./atom:title")
          :author (if author author (parse-author ($x:node (xp "./atom:author") entry)))
          :summary (text "./atom:summary")
          :content (strip-html (text "./atom:content"))
          :scope (text "./mtdn:scope")
          :attachments (map parse-attachment-tag ($x:attrs* (xp "./atom:link[@rel='enclosure']") entry))
          :mentioned-user-urls (map map->ref (get-objects entry "mentioned" :person))
          :in-reply-to (map :href ($x:attrs* (xp "./thr:in-reply-to") entry))
          :html-url (map->ref ($x:attrs? (xp "./atom:link[@rel='alternate' and @type='text/html']") entry))
          :atom-url (map->ref ($x:attrs? (xp "./atom:link[@rel='self' and @type='application/atom+xml']") entry))})))))

(defn parse-feed
  [feed]
  (with-masto-ns
    (let [text (text-getter feed)
          attr (attr-getter feed)
          author (->
                  (parse-author ($x:node (xp "./atom:author") feed))
                  (assoc :hub-url (attr "./atom:link[@rel='hub']" :href))
                  (assoc :salmon-url (attr "./atom:link[@rel='salmon']" :href))
                  (assoc :atom-url (text "./atom:id")))
          entries (map #(parse-entry % author) ($x:node* (xp "./atom:entry") feed))]
      (c/map-> Feed {:account author :posts entries}))))
        
(defn parse
  [inp]
  (with-masto-ns
    (let [inp (read-doc inp)
          feed-node ($x:node? (xp "./atom:feed") inp)]
      (if feed-node
        (parse-feed feed-node)
        (map parse-entry ($x:node* (xp "./atom:entry") inp))))))

(defn author-tree
  [account]
  (let [account (c/expand account)]
    [:atom:author
      [:atom:id (c/atom-id-for account)]
      [:activity:object-type (:person types)]
      [:atom:uri (:html-url account)]
      [:atom:name (:username account)]
      [:atom:email (:qualified-username account)]
      [:atom:summary (:bio account)]
      [:atom:link {:rel "alternate" :type "text/html" :href (:html-url account)}]
      [:atom:link {
        :rel "avatar" :type (:av-type account)
        :media:width (:av-width account) :media:height (:av-height account)
        :href (:av account)}]
      [:atom:link {
        :rel "header" :type (:header-image-type account)
        :media:width (:header-image-width account) :media:height (:header-image-height account)
        :href (:header-image account)}]
      [:poco:preferredUsername (:username account)]
      [:poco:displayName (:display-name account)]
      [:poco:note (:bio account)]
      [:mtdn:scope (:scope account)]]))
  
(defn post-tree
  ([post] (post-tree post false))
  ([post inner?]
    (let [post (c/expand post)]
      [:atom:post (if inner? {} (assoc xmlns-attrs "xmlns" (:atom namespaces)))
        [:atom:id (c/atom-id-for post)]
        [:atom:published (to-iso-string (:published post))]
        [:atom:updated (to-iso-string (:updated post))]
        [:atom:title (:title post)]
        (if-not inner?
          (author-tree (:author post)))
        [:activity:object-type (:comment types)]
        [:activity:verb (:post verbs)]
        [:atom:summary (:summary post)]
        [:atom:content {:type "html"} (format "<pre>%s</pre>" (escape-html (:content post)))]
        (for [a (:attachments post)]
          [:atom:link {
            :rel "enclosure"
            :href (:href a)
            :type (:type a)
            :length (:length a)}])
        (for [u (:mentioned-user-urls post)]
          [:atom:link {
            :rel "mentioned"
            :os:object-type (:person types)
            :href (c/url-for u)}])
        [:mtdn:scope (:scope post)]
        [:atom:link {:rel "alternate" :type "text/html" :href (:html-url post)}]
        [:atom:link {:rel "self" :type "application/atom+xml" :href (:atom-url post)}]
        (for [v (:in-reply-to post)]
          [:thr:in-reply-to {:ref (c/atom-id-for v) :href (c/url-for v)}])])))

(defn feed-tree
  [account posts]
  (let [account (c/expand account)
        posts (map c/expand posts)]
    (assert (all? (map #(= (:qualified-username %) (:qualified-username account)) posts)))
    (xmlns-tag :feed :atom
      [:atom:id (:atom-url account)]
      [:atom:title (format "feed for %s" (:qualified-username account))]
      [:atom:subtitle (:bio account)]
      [:atom:logo (:av account)]
      [:atom:updated (to-iso-string (reduce max (map :updated posts)))]
      (if (:hub-url account)
        [:atom:link {:rel "hub" :href (:hub-url account)}])
      (if (:salmon-url account)
        (list
          [:atom:link {:rel "salmon" :href (:salmon-url account)}]
          [:atom:link {:rel "http://salmon-protocol.org/ns/salmon-replies" :href (:salmon-url account)}]
          [:atom:link {:rel "http://salmon-protocol.org/ns/salmon-mention" :href (:salmon-url account)}]))
      (author-tree account)
      (map #(post-tree % true) posts))))
    
(defn render-author
  [author]
  (render-xml (author-tree author)))

(defn render-post
  [post]
  (render-xml (post-tree post)))

(defn render-feed
  [account posts]
  (render-xml (feed-tree account posts)))
