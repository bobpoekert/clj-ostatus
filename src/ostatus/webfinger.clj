(ns ostatus.webfinger
  (:import [java.util.regex Pattern Matcher])
  (:require [ostatus.salmon :as sl]
            [ostatus.types :as c]
            [cheshire.core :refer [parse-string generate-string]]))

(defn- re-group
  [^java.util.regex.Pattern pattern ^String s]
  (if s
    (let [^java.util.regex.Matcher m (.matcher pattern s)]
      (if (.matches m)
        (.group m 1)))))

(def ^Pattern acct-url-pattern #"(?:acct:)?(.+@.+)")
(defn decode-acct-url
  [urls]
  (first (filter identity (map #(re-group acct-url-pattern %) urls))))

(def ^Pattern acct-url-username-pattern #"(?:acct:)?(.+?)@.*?")
(defn username-from-acct-url
  [^String url]
  (re-group acct-url-username-pattern url))

(def ^Pattern alias-username-pattern #"^username:(.*)")
(defn username-from-aliases
  [aliases]
  (first
    (filter identity
      (for [a aliases]
        (re-group alias-username-pattern a)))))

(def ^Pattern acct-url-parts-pattern #"(?:acct:)?(.+?)@(.*)")
(defn webfinger-url
  [qualified-username]
  (let [m (.matcher acct-url-parts-pattern qualified-username)
        _ (.matches m)
        username (.group m 1)
        host (.group m 2)]
    (format "https://%s/.well-known/webfinger?resource=acct:%s@%s" host username host)))

(defn decode-webfinger-json-map
  [json-map]
  (let [qualified-username (decode-acct-url (cons (:subject json-map) (:aliases json-map)))]
    (c/map-> Account
      (reduce
        (fn [state link]
          (case (:rel link)
            "http://webfinger.net/rel/profile-page" (if (= (:type link) "text/html") (assoc state :html-url (:href link)) state)
            "http://schemas.google.com/g/2010#updates-from" (if (= (:type link) "application/atom+xml")
                                                              (assoc state :atom-url (:href link)))
            "salmon" (assoc state :salmon-url (:href link))
            "magic-public-key" (assoc state :salmon-public-key (sl/unpack-magic-key (:href link)))
            "describedby" (if (= (:type link) "application/rdf+xml") (assoc state :foaf-url (:href link)) state)
            "http://ostatus.org/schema/1.0/subscribe" (assoc state :subscribe-url-pattern (:template link))
            state))
        {
            :aliases (filter (fn [^String v] (not (re-matches alias-username-pattern v))) (:aliases json-map))
            :username (or (username-from-aliases (:aliases json-map)) (username-from-acct-url qualified-username))
            :qualified-username qualified-username}
        (:links json-map)))))

(defn decode-webfinger-json
  [blob]
  (decode-webfinger-json-map (parse-string blob true)))

(defn encode-webfinger-json-map
  [account]
  {
    :subject (format "acct:%s" (:qualified-username account))
    :aliases (into [(format "username:%s" (:username account))]
              (get account :aliases []))
    :links (filter identity [
      {:rel "http://webfinger.net/rel/profile-page"
       :type "text/html"
       :href (:html-url account)}
      (if (:atom-url account)
        {:rel "http://schemas.google.com/g/2010#updates-from"
         :type "application/atom+xml"
         :href (:atom-url account)})
      (if (:salmon-url account)
        {:rel "salmon"
         :href (:salmon-url account)})
      (if (:salmon-public-key account)
        {:rel "magic-public-key"
         :href (sl/pack-magic-key-url (:salmon-public-key account))})
      (if (:subscribe-url-pattern account)
        {:rel "http://ostatus.org/schema/1.0/subscribe"
         :template (:subscribe-url-pattern account)})])})

(defn encode-webfinger-json
  [account]
  (generate-string (encode-webfinger-json-map account)))
