(ns ostatus.atom
  (:require [clj-xpath.core :refer :all]
            [ostatus.core :as c]
            [ostatus.util :refer :all]
            [hiccup.core :as h]))

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
      (format "./atom:link[rel='%s' and os:object-type='%s']"
        rel (get u/types typ))
      node)))

(defn parse-entry
  [entry]
  (with-masto-ns
    (let [text (text-getter entry)
          attr (attr-getter entry)]
      (c/map->Post {
        :published (u/from-iso-string (text "./atom:published"))
        :updated (u/from-iso-string (text "./atom:updated"))
        :title (text "./atom:title")
        :author (parse-author ($x:node "./atom:author" entry))
        :summary (text "./atom:summary")
        :content (u/strip-html (text "./atom:content"))
        :scope (text "./mtdn:scope")
        :mentioned-user-urls (map :href (get-objects entry "mentioned" :person))
        :in-reply-to (map :href ($x:attrs* "./thr:in-reply-to" entry))
        :html-url (attr "./atom:link[@rel='alternate' and @type='text/html']" :href)
        :atom-url (attr "./atom:link[@rel='self' and @type='application/atom+xml']" :href)}))))

(defn parse
  [inp]
  (with-masto-ns
    (let [inp (read-doc inp)]
      (map parse-entry ($x:node* "./atom:entry" inp)))))
