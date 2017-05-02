(ns ostatus.salmon
  (:import [javax.crypto SecretKey SecretKeyFactory]
           [javax.crypto.spec PBEKeySpec]
           [java.security SecureRandom KeyPairGenerator KeyPair PublicKey PrivateKey KeyFactory]
           [java.security.interfaces RSAPublicKey]
           [java.security.spec RSAPublicKeySpec]
           [java.math BigInteger]
           [java.util.regex Pattern Matcher]
           [java.io DataOutputStream ByteArrayOutputStream DataInputStream ByteArrayInputStream])
  (:require [ostatus.types :as c]))

(set! *warn-on-reflection* true)

(def ^java.util.Base64$Decoder bdecoder (java.util.Base64/getUrlDecoder))
(def ^java.util.Base64$Encoder bencoder (java.util.Base64/getUrlEncoder))

(defn ^BigInteger decode-number
  [^String inp]
  (BigInteger. (.decode bdecoder inp)))

(defn ^String encode-number
  [^BigInteger inp]
  (.encodeToString bencoder (.toByteArray inp)))
 
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
