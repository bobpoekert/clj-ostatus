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
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.generators :refer [string-from-regex]]))

(def test-post 
  (c/map->Post {
    :published (u/from-iso-string "2017-04-22T00:11:53Z")
    :updated (u/from-iso-string "2017-04-22T00:11:53Z")
    :title "New status by parataxis"
    :content "@djsundog did you try to sync timecodes with external metadata? because we couldn't get that to work well at twitch until we put them in the RTMP stream with the RTMP stream's timecodes"
    :summary "fediverse noodling again"
    :attachments []
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

;<link rel="enclosure" type="image/jpeg" length="281469" href="https://cybre.s3-us-west-2.amazonaws.com/media_attachments/files/000/055/229/original/a8fc99f1994eb86b.jpg"/>

(def image-test-post
  (c/map->Post
    {:published 1493526542,
     :attachments [
        (c/map->Attachment {
          :type "image/jpeg"
          :length 281469
          :href "https://cybre.s3-us-west-2.amazonaws.com/media_attachments/files/000/055/229/original/a8fc99f1994eb86b.jpg"})]
     :author (c/map->Account
     {:username "lain_iwakura",
      :uri "https://cybre.space/users/lain_iwakura",
      :qualified-username "lain_iwakura@cybre.space",
      :html-url "https://cybre.space/@lain_iwakura",
      :av
      "https://cybre.s3-us-west-2.amazonaws.com/accounts/avatars/000/009/680/original/f6e4d90b4c640bb2.png",
      :header-image
      "https://cybre.s3-us-west-2.amazonaws.com/accounts/headers/000/009/680/original/e43c8db3df8b573f.jpg",
      :av-width 120,
      :av-height 120,
      :header-image-width 700,
      :header-image-height 335,
      :av-type "image/png",
      :header-image-type "image/jpeg",
      :display-name "Lain Iwakura (岩倉 玲音)",
      :bio (java.net.URLDecoder/decode "%EF%BD%81%CC%AD%CC%96%CC%A9%EF%BD%8E%CC%B0%CC%AB%EF%BD%84%CC%B5%CC%9F%E3%80%80%CD%8E%CC%BC%CC%96%CC%AA%CC%9C%CC%B2%EF%BD%99%CC%B6%CC%AA%CC%B0%CC%A4%CC%9D%CD%96%EF%BD%8F%CC%99%CC%B3%EF%BD%95%CD%8E%CC%AE%CD%94%CC%96%CC%B3%CC%9F%E3%80%80%CC%A4%EF%BD%84%CD%98%CC%B0%CC%B2%CC%A9%CD%93%EF%BD%8F%CD%87%CC%A6%CD%88%EF%BD%8E%CC%B7%CC%98%CD%93%EF%BC%87%CC%B1%CC%B0%CC%9C%CC%96%CC%B3%EF%BD%94%CC%AA%CC%A5%E3%80%80%CC%9D%CC%B1%CD%96%CC%B9%CD%8D%CD%9A%EF%BD%93%CD%A2%CD%99%EF%BD%85%CC%99%CC%B9%EF%BD%85%CD%A0%CC%B9%CC%A3%EF%BD%8D%CC%A5%CD%89%CC%AC%CC%B2%CC%B9%E3%80%80%CD%9F%CC%B0%CC%AE%CC%AE%CC%BA%EF%BD%94%EF%BD%8F%CD%A1%E3%80%80%D2%89%CC%BA%CD%87%CC%BA%EF%BD%95%CC%B7%CC%B1%CD%8D%CD%96%CC%B3%CC%B2%EF%BD%8E%CC%BB%CC%A6%CC%BB%CC%9C%CD%87%EF%BD%84%CC%B7%CD%96%CC%AC%EF%BD%85%CC%A3%EF%BD%92%EF%BD%93%CC%98%CC%96%CC%A3%CC%BC%EF%BD%94%CC%96%CD%9A%CD%94%CC%AC%CC%AA%EF%BD%81%CC%A0%CC%A5%CD%8D%EF%BD%8E%CC%A7%CD%93%CC%A4%EF%BD%84%CD%9E%CC%9F%CD%8D%CD%93%CC%B2%CC%9E%CD%9A" "UTF-8")
      :scope "public"}),
     :content "https://cybre.space/media/G6EraV6UeheFA3DXsKE",
     :atom-url "https://cybre.space/users/lain_iwakura/updates/30007.atom",
     :html-url "https://cybre.space/users/lain_iwakura/updates/30007",
     :updated 1493526542,
     :title "New status by lain_iwakura",
     :summary "https://cybre.space/media/G6EraV6UeheFA3DXsKE",
     :scope "public",
     :mentioned-user-urls [],
     :in-reply-to []}))


(def gen-posts [
  (c/expand (c/map->Post {:published 0, :author (c/map->Account {:username "", :uri "http://-.-!", :qualified-username "!@!.!", :html-url "http://-.-!", :av "http://-.-!", :header-image "http://-.-!"}), :content ""}))])
(deftest masto-status-atom
  (testing "post.atom parses correctly"
    (with-open [inp (io/input-stream "test_data/post.atom")]
      (is (= (a/parse inp) [test-post]))))
  (testing "test post roundtrips"
    (is (= (a/parse (a/render-post test-post)) [test-post]))
    (is (= (a/parse (a/render-post image-test-post)) [image-test-post]))
    (doseq [gen-post gen-posts]
      (is (= (a/parse (a/render-post gen-post)) [gen-post])))))

(def ^java.util.regex.Pattern test-re #"[^\.]+@[^\.]+\..+")
(defspec regex-roundtrip
  1000
  (prop/for-all [^CharSequence v (string-from-regex test-re)]
    (.matches (.matcher test-re v))))

(defspec atom-roundtrip
  100
  (prop/for-all [post (sp/gen :ostatus.types/Post)]
    (let [post (assoc post :author (c/map->Account (:author post)))
          post (assoc post :attachments (map c/map->Attachment (:attachments post)))
          post (c/map->Post post)
          post (c/expand post)]
      (= (a/parse (a/render-post post)) [post]))))
