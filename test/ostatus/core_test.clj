(ns ostatus.core-test
  (:require [clojure.test :refer :all]
            [ostatus.types :as c]
            [ostatus.util :as u]
            [ostatus.atom :as a]
            [clojure.java.io :as io]
            [clojure.spec :as sp]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def test-post 
  (c/map->Post {
    :published (u/from-iso-string "2017-04-22T00:11:53Z")
    :updated (u/from-iso-string "2017-04-22T00:11:53Z")
    :title "New status by parataxis"
    :content "@djsundog did you try to sync timecodes with external metadata? because we couldn't get that to work well at twitch until we put them in the RTMP stream with the RTMP stream's timecodes"
    :summary "fediverse noodling again"
    :author (c/map->Account {
      :uri "https://icosahedron.website/users/parataxis"
      :username "parataxis"
      :qualified-username "parataxis@icosahedron.website"
      :bio "best mesothelioma lawyer dallas truck accident lawyer truck accident lawyer houston loisville car accident lawyer san diego water damage. @bobpoekert on twitter"
      :html-url "https://icosahedron.website/@parataxis"
      :av "https://icosahedron.website/system/accounts/avatars/000/003/206/original/e7d13c2dd2a8b28e.jpeg?1491246678"
      :av-width 120
      :av-height 120
      :av-type "image/jpeg"
      :header-image "https://icosahedron.website/system/accounts/headers/000/003/206/original/bb4fefe827c66aa8.jpeg?1491246678"
      :header-image-width 700
      :header-image-height 335
      :header-image-type "image/jpeg"
      :display-name "bob ✅"
      :scope "public"})
    :scope "public"
    :mentioned-user-urls ["https://toot-lab.reclaim.technology/users/djsundog"]
    :in-reply-to ["https://toot-lab.reclaim.technology/users/djsundog/updates/1076"]
    :html-url "https://icosahedron.website/users/parataxis/updates/35422"
    :atom-url "https://icosahedron.website/users/parataxis/updates/35422.atom"}))

(defn diff-records
  [a b]
  (doseq [[k v] a]
    (let [v2 (get b k)]
      (if-not (= v v2)
        (if (and (map? v) (map? v2))
          (diff-records v v2)
          (prn [k v v2]))))))

(deftest masto-status-atom
  (testing "post.atom parses correctly"
    (with-open [inp (io/input-stream "test_data/post.atom")]
      (is (= (a/parse inp) [test-post]))))
  (testing "test post roundtrips"
    (is (= (a/parse (a/render-post test-post)) [test-post]))))

(comment
(defspec atom-roundtrip
  10000
  (prop/for-all [post (sp/gen :ostatus.types/Post)]
    (let [post (c/expand post)]
      (= (a/parse (a/render-post post)) post)))))
