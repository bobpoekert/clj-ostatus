(ns ostatus.foaf
  (:require [ostatus.util :refer :all]
            [clj-xpath.core :refer :all]
            [clojurewerkz.urly.core :as uu]))

(def foaf-namespaces {
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :geo "http://www.w3.org/2003/01/geo/wgs84_pos#"
    :bio "http://purl.org/vocab/bio/0.1/"
    :sioc "http://rdfs.org/sioc/ns#"
    :foaf "http://xmlns.com/foaf/0.1/"})

(def foaf-context (xpath-context foaf-namespaces))

(defn parse-agent
  [agent-tag]
  (with-xpath-ns foaf-context
    (let [text (text-getter agent-tag)
          attr (attr-getter agent-tag)
          get-resources (fn [query]
                          (->>
                            ($x:attrs* (xp query) agent-tag)
                            (map :rdf:resource)
                            (filter identity)
                            (map uu/without-fragment)))]
      {:ids (cons (uu/without-fragment (:rdf:about (attrs agent-tag))) (get-resources ".//sioc:account_of"))
       :username (or (text "./foaf:name") (text "./foaf:account/foaf:OnlineAccount/foaf:accountName"))
       :html-url (or (attr "./foaf:homepage" :rdf:resource)
                     (attr "./foaf:account/foaf:OnlineAccount/foaf:accountProfilePage" :rdf:resource))
       :av (:rdf:about (first ($x:attrs* (xp "./foaf:img/foaf:Image") agent-tag)))
       :bio (text "./bio:olb")
       :following (get-resources "./foaf:account/foaf:OnlineAccount/sioc:follows")
       :followers (get-resources "./foaf:knows")})))
   
(defn argmax
  [k s]
  (loop [m Integer/MIN_VALUE
         mk nil
         [h & t] s]
    (if h
      (let [m2 (k h)]
        (if (> m2 m)
          (recur m2 h t)
          (recur m mk t)))
      mk)))

(defn parse-foaf
  [blob]
  (with-xpath-ns foaf-context
    (let [tree (read-doc blob)
          attr (attr-getter tree)
          agents (map parse-agent ($x:node* (xp "/rdf:RDF/foaf:Agent") tree))
          agent-index (reduce merge (for [a agents id (:ids a)] {id a}))
          target-agent (argmax #(+ (count (:followers %)) (count (:following %))) agents)]
      (-> target-agent
        (assoc :following (map #(get agent-index %) (:following target-agent)))
        (assoc :followers (map #(get agent-index %) (:followers target-agent)))))))
