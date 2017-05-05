(ns ostatus.mastodon-test
  (:require [zweikopf.core :as z]
            [clojure.test :refer :all]
            [ostatus.types :as c]
            [ostatus.util :as u]
            [ostatus.atom :as a]
            [ostatus.salmon :as sl]
            [ostatus.webfinger :as wf]
            [cheshire.core :refer [parse-string]]
            [clojure.java.io :as io]
            [clojure.spec :as sp]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(comment
(z/init-ruby-context)
(z/set-gem-path "test_data/mastodon/vendor/gem_home")

(z/ruby-require "test_data/mastodon")
(def mastodon (z/ruby-eval "MastodonBridge"))

(defn cm
  [method & args]
  (z/clojurize
    (apply z/call-ruby mastodon method
      (map z/rubyize args))))

)
