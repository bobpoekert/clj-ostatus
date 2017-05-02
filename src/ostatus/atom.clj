(ns ostatus.atom
  (:require [clj-xpath.core :refer :all]
            [ostatus.types :as c]
            [ostatus.util :refer :all]))

(set! *warn-on-reflection* true)

(defn parse-author
  [author]
  (with-masto-ns
    (let [av ($x:attrs? "./atom:link[@rel='avatar']" author)
          header ($x:attrs? "./atom:link[@rel='header']" author)
          text (text-getter author)
          attr (attr-getter author)]
      (c/map->Account {
        :uri (text "./atom:uri")
        :username (text "./atom:name")
        :qualified-username (text "./atom:email")
        :bio (text "./atom:summary")
        :html-url (attr "./atom:link[@rel='alternate' and @type='text/html']" :href)
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
  (c/map->Attachment {
    :href (:href attachment)
    :type (:type attachment)
    :length (number (:length attachment))}))

(defn parse-entry
  [entry]
  (with-masto-ns
    (let [text (text-getter entry)
          attr (attr-getter entry)]
      (c/map->Post {
        :published (from-iso-string (text "./atom:published"))
        :updated (from-iso-string (text "./atom:updated"))
        :title (text "./atom:title")
        :author (parse-author ($x:node (xp "./atom:author") entry))
        :summary (text "./atom:summary")
        :content (strip-html (text "./atom:content"))
        :scope (text "./mtdn:scope")
        :attachments (map parse-attachment-tag ($x:attrs* (xp "./atom:link[@rel='enclosure']") entry))
        :mentioned-user-urls (map map->ref (get-objects entry "mentioned" :person))
        :in-reply-to (map :href ($x:attrs* (xp "./thr:in-reply-to") entry))
        :html-url (map->ref ($x:attrs? (xp "./atom:link[@rel='alternate' and @type='text/html']") entry))
        :atom-url (map->ref ($x:attrs? (xp "./atom:link[@rel='self' and @type='application/atom+xml']") entry))}))))

(defn parse
  [inp]
  (with-masto-ns
    (let [inp (read-doc inp)]
      (map parse-entry ($x:node* "./atom:entry" inp)))))

(defn author-tree
  [account]
  (let [account (c/expand account)]
    [:atom:author
      [:atom:id (c/atom-id-for account)]
      [:activity:object-type (:person types)]
      [:atom:uri (:uri account)]
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
  [post]
  (let [post (c/expand post)]
    (xmlns-tag :entry :atom
      [:atom:id (c/atom-id-for post)]
      [:atom:published (to-iso-string (:published post))]
      [:atom:updated (to-iso-string (:updated post))]
      [:atom:title (:title post)]
      (author-tree (:author post))
      [:activity:object-type (:comment types)]
      [:activity:verb (:post verbs)]
      [:atom:summary (:summary post)]
      [:atom:content {:type "html"} (format "<pre>%s</pre>" (:content post))]
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
        [:thr:in-reply-to {:ref (c/atom-id-for v) :href (c/url-for v)}]))))

(defn render-author
  [author]
  (render-xml (author-tree author)))

(defn render-post
  [post]
  (render-xml (post-tree post)))
