(ns ostatus.types
  (:import [javax.crypto SecretKey SecretKeyFactory]
           [javax.crypto.spec PBEKeySpec]
           [java.security SecureRandom KeyPairGenerator KeyPair PublicKey PrivateKey])
  (:require [clojure.spec :as sp]
           [clojure.string :as s]
           [ostatus.util :refer [hash-string]]
           [clojure.test.check.generators :as gen]
           [com.gfredericks.test.chuck.generators :refer [string-from-regex]]))

(set! *warn-on-reflection* true)

(defprotocol Expand
  "Takes a map that has optional keys and generates default values from the required keys"
  (expand [v]))

(extend-protocol Expand
  Object
  (expand [v] v)
  nil
  (expand [v] nil))

(defprotocol Validate
  "Takes a map and throws an exception if it's invalid in some way"
  (validate! [v]))

(extend-protocol Validate
  Object
  (validate! [v] nil)
  nil
  (validate! [v] nil))

(def rec-keys (atom {}))

(def types-ns *ns*)
(defmacro map->
  [typename m]
  (let [constructor (if (map? m)
                      (cons (symbol (str types-ns) (format "->%s" (name typename)))
                        (for [k (get @rec-keys typename)]
                          (get m k)))
                      (list (symbol (str types-ns) (format "map->%s" (name typename))) m))]
    `(let [v# ~constructor]
      (validate! v#)
      v#)))

(defn strip-ns
  [kw]
  (keyword (name kw)))

(defn- gen-expand-impl
  [argname typename gen-map ks]
  `(~(symbol (format "->%s" (name typename)))
    ~@(for [k ks]
        (let [k (keyword (name k))]
          (list 'expand
            (if (get gen-map k)
              `(let [v# (get ~argname ~k)]
                (if (nil? v#) (~(get gen-map k) ~argname) v#))
              `(get ~argname ~k)))))))

(defmacro specrec
  [nom & spec]
  (let [specmap (zipmap (take-nth 2 spec) (take-nth 2 (rest spec)))
        xp-keys (take-nth 2 (get specmap :xp []))
        spec-spec (list :req-un (get specmap :req [])
                        :opt-un (vec (concat (get specmap :opt []) xp-keys)))
        gens (:xp specmap)
        genkeys (for [k xp-keys] (gensym (name k)))
        genvals (take-nth 2 (rest (get specmap :xp [])))
        argsym (gensym "expand")
        specname (keyword (str *ns*) (name nom))
        specvals (concat
                  (get specmap :req [])
                  (get specmap :opt [])
                  xp-keys)]
    (swap! rec-keys (fn [v] (assoc v nom (map strip-ns specvals))))
    `(do
      ~@(map (fn [k v] `(def ~k ~v)) genkeys genvals)
      ;; HACK: make optional keys nilable
      ~@(for [k (get specmap :opt [])]
        `(sp/def ~k (sp/nilable (sp/get-spec ~k))))
      (sp/def ~specname (sp/keys ~@spec-spec))
      (defrecord ~(symbol (name nom)) ~(vec (map #(symbol (name %)) specvals))
        Validate
        (validate! [~argsym]
          (sp/assert ~specname ~argsym))
        Expand
        (expand [~argsym]
          (let [~argsym ~(gen-expand-impl argsym nom (zipmap (map strip-ns xp-keys) genkeys) specvals)]
            (sp/assert ~specname ~argsym)
            ~argsym))))))
          
          
;; types: account
;; , post
;; , follow-action
;; , unfollow-action
;; , authorize-action
;; , reject-action
;; , friend-request-action
;; , block-action
;; , unblock-action
;; , share-action
;; , fav-action
;; , unfav-action

(defn clip-string
  [max-len ^String s]
  (if (or (nil? s) (<= (.length s) max-len))
    s
    (str
      (.substring s 0 (dec max-len))
      "…")))

(defn returns
  [v]
  (fn [& a] v))

(defprotocol UrlFor
  (url-for [v]))

(defprotocol AtomIdFor
  (atom-id-for [v]))

(defprotocol Unref
  (unref [v]))

(extend-protocol AtomIdFor
  Object
  (atom-id-for [v] (hash-string (.hashCode v))))

(extend-protocol Unref
  Object
  (unref [v] v))

(extend-type String
  UrlFor
  (url-for [u] u)
  AtomIdFor
  (atom-id-for [u]
    (hash-string u)))

(defrecord AtomRef [ref href]
  UrlFor
  (url-for [r] (:href r))
  AtomIdFor
  (atom-id-for [r] (:ref r))
  Unref
  (unref [v] (:href v))
  Object
  (toString [r] (:href r)))



(defn matches-re?
  [^java.util.regex.Pattern re]
  (sp/with-gen
    (fn [v]
      (let [v (unref v)]
        (if (nil? v)
          false
          (.matches (.matcher re ^CharSequence v)))))
    (fn [] (string-from-regex re))))

(defn one-of?
  [vs]
  (sp/with-gen
    (fn [v]
      (boolean (some #(= % v) vs)))
    (fn [] (gen/elements vs))))

(def url? (matches-re? #"https?:\/\/[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#\[\]@!\$&'\(\)\*\+,;=.]+"))
(def email? (matches-re? #"[\w\-\._~:/?#\[\]@!\$&'\(\)\*\+,;=.]+@[\w\-\._~:/?#\[\]@!\$&'\(\)\*\+,;=.]+\.[\w\-\._~:/?#\[\]@!\$&'\(\)\*\+,;=.]+"))
(def epoch-int? (sp/with-gen integer? #(gen/large-integer* {:min -32513966177 :max 32513966177})))

(defn ^KeyPair rsa-keypair
  []
  (let [gen (KeyPairGenerator/getInstance "RSA")]
    (.initialize gen 2048)
    (.genKeyPair gen)))

(def test-keypair
  (delay (rsa-keypair)))

(def ^{:dynamic true} *generate-test-keys* false)
(def public-key?
  (sp/with-gen
    #(or (nil? %) (isa? PublicKey %))
    #(gen/return (if *generate-test-keys* 
                  (.getPublic ^KeyPair @test-keypair)
                  nil))))

(sp/def ::username string?)
(sp/def ::id url?)
(sp/def ::display-name string?)
(sp/def ::qualified-username email?)
(sp/def ::html-url url?)
(sp/def ::av url?)
(sp/def ::header-image url?)
(sp/def ::av-width integer?)
(sp/def ::av-height integer?)
(sp/def ::header-image-width integer?)
(sp/def ::header-image-height integer?)
(sp/def ::av-type string?)
(sp/def ::header-image-type string?)
(sp/def ::bio string?)
(sp/def ::scope (one-of? ["public" "private"]))
(sp/def ::salmon-url url?)
(sp/def ::salmon-public-key public-key?)
(sp/def ::aliases (sp/coll-of url?))
(sp/def ::atom-url url?)
(sp/def ::subscribe-url-pattern url?)
(sp/def ::hub-url url?)

(specrec Account
  :req [::username ::qualified-username ::html-url]
  :opt [::av ::av-width ::av-height ::av-type
        ::header-image ::header-image-width ::header-image-height ::header-image-type
        ::salmon-url ::salmon-public-key
        ::atom-url ::subscribe-url-pattern ::hub-url ::foaf-url]
  :xp [::display-name :username
       ::aliases (returns [])
       ::bio (returns "")
       ::scope (returns "public")])

(extend-type Account
  UrlFor
  (url-for [a] (:html-url a))
  AtomIdFor
  (atom-id-for [a]
    (hash-string (:html-url a))))

(sp/def ::href url?)
(sp/def ::type (matches-re? #"\w+\/\w+"))
(sp/def ::length integer?)

(specrec Attachment
  :req [::href]
  :opt [::type ::length])

(sp/def ::author ::Account)
(sp/def ::published epoch-int?)
(sp/def ::updated epoch-int?)
(sp/def ::content string?)
(sp/def ::title string?)
(sp/def ::summary string?)
(sp/def ::mentioned (sp/coll-of ::Post))
(sp/def ::in-reply-to (sp/coll-of url?))
(sp/def ::atom-url url?)
(sp/def ::mentioned-user-urls (sp/coll-of url?))
(sp/def ::attachments (sp/coll-of ::Attachment))

(specrec Post
  :req [::published ::author ::content]
  :opt [::atom-url ::html-url]
  :xp [::updated :published
        ::title #(format "new status by %s" (:username (:author %)))
        ::summary #(clip-string 140 (:content %))
        ::scope (returns "public") 
        ::attachments (returns [])
        ::mentioned-user-urls (returns [])
        ::in-reply-to (returns [])])

(extend-type Post
  UrlFor
  (url-for [p] (:html-url p))
  AtomIdFor
  (atom-id-for [p]
    (hash-string [(atom-id-for (:author p)) (:published p) (:content p)])))

(sp/def ::account ::Account)
(sp/def ::posts (sp/coll-of ::Post))
(specrec Feed
  :req [::account ::posts])
