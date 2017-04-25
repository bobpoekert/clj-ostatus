(ns ostatus.core
  (:require [clojure.spec :as sp]
           [clojure.string :as s]
           [com.gfredericks.test.chuck.generators :refer [string-from-regex]]))

(set! *warn-on-reflection* true)

(defprotocol Expand
  "Takes a map that has optional keys and generates default values from the required keys"
  (expand [v]))

(extend-protocol Expand
  Object
  (expand [v] v))

(defn- gen-impl
  [argname gens]
  (let [tn (gensym "transient")]
    `(let [~tn (transient ~argname)]
      ~@(for [[k gen] (partition 2 gens)]
        `(if (nil? (get ~tn ~k)) (assoc! ~tn ~k (expand (~gen ~argname)))))
      (persistent! ~tn))))

(defmacro specrec
  [nom & spec]
  (let [specmap (zipmap (take-nth 2 spec) (take-nth 2 (rest spec)))
        xp-keys (take-nth 2 (get specmap :xp []))
        spec-spec (list :req (get specmap :req [])
                        :opt (vec (concat (get specmap :opt []) xp-keys)))
        gens (:xp specmap)
        genkeys (for [k xp-keys] (gensym (name k)))
        genvals (take-nth 2 (rest (get specmap :xp [])))
        argsym (gensym "expand")
        specname (keyword (str *ns*) (name nom))
        specvals (concat
                  (get specmap :req [])
                  (get specmap :opt [])
                  xp-keys)]
    `(do
      ~@(map (fn [k v] `(def ~k ~v)) genkeys genvals)
      (sp/def ~specname (sp/keys ~@spec-spec))
      (defrecord ~(symbol (name nom)) ~(vec (map #(symbol (name %)) specvals))
        Expand
        (expand [~argsym]
          (assert (sp/valid? ~specname ~argsym))
          (let [res# ~(gen-impl argsym (interleave xp-keys genkeys))]
            (assert (sp/valid? ~specname res#))
            res#))))))
          
          
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
  (if (<= (.length s) max-len)
    s
    (str
      (.substring s 0 (dec max-len))
      "…")))

(defn returns
  [v]
  (fn [& a] v))

(defn matches-re?
  [^java.util.regex.Pattern re]
  (sp/with-gen
    (fn [^CharSequence v]
      (.matches (.matcher re v)))
    (string-from-regex re)))

(def url? (matches-re? #"(?:http(s)?:\/\/)?[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#\[\]@!\$&'\(\)\*\+,;=.]+"))

(sp/def ::username string?)
(sp/def ::id url?)
(sp/def ::uri url?)
(sp/def ::display-name string?)
(sp/def ::qualified-username (matches-re? #"[^\.]+@[^\.]+\..+"))
(sp/def ::html-url url?)
(sp/def ::av url?)
(sp/def ::header-image url?)

(specrec Account
  :req [::username ::id ::qualified-username ::html-url ::av ::header-image]
  :xp [::display-name ::username
       ::bio (returns "")
       ::scope (returns "public")])

(sp/def ::author ::Account)
(sp/def ::published number?)
(sp/def ::content string?)
(sp/def ::updated number?)
(sp/def ::title string?)
(sp/def ::summary string?)
(sp/def ::mentioned (sp/coll-of ::Post))
(sp/def ::in-reply-to (sp/coll-of ::Post))

(specrec Post
  :req [::published ::author ::content]
  :xp [::updated ::published
        ::title #(format "new status by %s" (::username (::author %)))
        ::summary #(clip-string 140 (::content %))
        ::scope (returns "public") 
        ::mentioned (returns [])
        ::in-reply-to (returns [])])
