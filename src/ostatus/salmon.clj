(ns ostatus.salmon
  (:import [javax.crypto SecretKey SecretKeyFactory]
           [javax.crypto.spec PBEKeySpec]
           [java.security SecureRandom KeyPairGenerator KeyPair PublicKey PrivateKey KeyFactory MessageDigest Signature]
           [java.security.interfaces RSAPublicKey]
           [java.security.spec RSAPublicKeySpec]
           [java.math BigInteger]
           [java.util.regex Pattern Matcher]
           [java.io DataOutputStream ByteArrayOutputStream DataInputStream ByteArrayInputStream]
           [java.util Arrays])
  (:require [ostatus.types :as c]
            [ostatus.util :as u]
            [cheshire.core :refer [parse-string generate-string]]))

(set! *warn-on-reflection* true)

(def ^java.util.Base64$Decoder bdecoder (java.util.Base64/getUrlDecoder))
(def ^java.util.Base64$Encoder bencoder (java.util.Base64/getUrlEncoder))

(defn ^BigInteger decode-number
  [^String inp]
  (BigInteger. 1 (.decode bdecoder inp)))

(defn ^String encode-number
  [^BigInteger inp]
  (.encodeToString bencoder (.toByteArray inp)))

(defn ^String encode-string
  [^String inp]
  (.encodeToString bencoder (.getBytes inp "UTF-8")))

(defn decode-string
  [^String inp]
  (.decode bdecoder inp))

(defn sha256
  [^String m]
  (let [d (MessageDigest/getInstance "SHA-256")]
    (.update d (.getBytes m "UTF-8"))
    (.digest d)))

(defn rsa-pubkey-from-numbers
  [^BigInteger modulus ^BigInteger exponent]
  (let [factory (KeyFactory/getInstance "RSA")
        spec (RSAPublicKeySpec. modulus exponent)]
    (.generatePublic factory spec)))

(def ^Pattern magic-key-pattern #"(?:data:application/magic-public-key,)?RSA\.([^\.]+)\.(.+)")
(defn ^PublicKey unpack-magic-key
  [^String inp]
  (let [^Matcher m (.matcher magic-key-pattern inp)]
    (if (.matches m)
      (let [modulus (decode-number (.group m 1))
            exponent (decode-number (.group m 2))]
        (rsa-pubkey-from-numbers modulus exponent)))))
    
(defn ^String pack-magic-key
  [^RSAPublicKey k]
  (format "RSA.%s.%s"
    (encode-number (.getModulus k))
    (encode-number (.getPublicExponent k))))

(defn ^String pack-magic-key-url
  [k]
  (format "data:application/magic-public-key,%s" (pack-magic-key k)))

(defn sign
  [data mime-type ^KeyPair key-pair]
  (let [algorithm "RSA-SHA256"
        encoding "base64url"
        data-b64 (encode-string data)
        type-b64 (encode-string mime-type)
        encoding-b64 (encode-string encoding)
        alg-b64 (encode-string algorithm)
        message-string (format "%s.%s.%s.%s" data-b64 type-b64 encoding-b64 alg-b64)
        sig (Signature/getInstance "SHA256withRSA")]
    (.initSign sig (.getPrivate key-pair))
    (.update sig (.getBytes message-string "UTF-8"))
    (encode-string (.sign sig))))

(defn xml-envilope
  [data mime-type key-pair]
  (let [sig (sign data mime-type key-pair)
        data-blob (encode-string data)]
    (u/render-xml
      [:env {:xmlns "http://salmon-protocol.org/ns/magic-env"}
        [:data {:type mime-type} data-blob]
        [:encoding "base64url"]
        [:alg "RSA-SHA256"]
        [:sig sig]])))

(defn json-envilope
  [data mime-type key-pair]
  (let [sig (sign data mime-type key-pair)
        data-blob (encode-string data)]
    (generate-string {
      "data" data-blob
      "data_type" mime-type
      "encoding" "base64url"
      "alg" "RSA-SHA256"
      "sigs" [{"value" sig}]})))
