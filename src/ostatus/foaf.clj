(ns ostatus.foaf
  (:require [ostatus.util :refer :all]
            [clj-xpath.core :refer :all]))

(def foaf-namespaces {
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :rdfs "http://www.w3.org/2000/01/rdf-schema#"
    :geo "http://www.w3.org/2003/01/geo/wgs84_pos#"
    :bio "http://purl.org/vocab/bio/0.1/"
    :sioc "http://rdfs.org/sioc/ns#"
    :foaf "http://.com/foaf/0.1/"})

(def foaf-context (xpath-context foaf-namespaces))

(defn parse-agent
  [agent-tag]
  (with-xpath-ns foaf-context
    (let [text (text-getter agent-tag)
          attr (attr-getter agent-tag)]
      {:id (:rdf:about (attrs agent-tag))
       :username (text "./name")
       :html-url (attr "./homepage" :rdf:resource)
       :av (:rdf:about (first ($x:attrs* (xp "./img/Image") agent-tag)))
       :bio (text "./bio:olb")
       :following (map :rdf:resource ($x:attrs* (xp "./account/OnlineAccount/soic:follows") agent-tag))
       :followers (map :rdf:resource ($x:attrs* (xp "./knows") agent-tag))})))
    
(defn parse-foaf
  [blob]
  (with-xpath-ns foaf-context
    (let [tree (read-doc blob)
          agents (map parse-agent ($x:node* (xp "//Agent") tree))
          agent-index (apply hash-map (for [a agents] [(:id a) a]))
          resolved-agents (for [a agents]
                            (-> a
                              (assoc :followers (map #(get agent-index %) (:followers a)))
                              (assoc :following (map #(get agent-index %) (:following a)))))]
      resolved-agents)))
