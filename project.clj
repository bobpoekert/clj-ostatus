(defproject ostatus "0.2"
  :description "An ostatus client"
  :url "https://github.com/bobpoekert/clj-ostatus"
  :license {:name "MIT License"}
  :profiles {:dev {:dependencies [[diff-eq "0.2.3"]
                                  [zweikopf "1.0.2"]]
                   :injections [(require 'diff-eq.core)
                                (diff-eq.core/diff!)]}}
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/test.check "0.9.0"]
                  [org.clojure/clojure "1.9.0-alpha15"]
                  [com.gfredericks/test.chuck "0.2.7"]
                  [com.github.kyleburton/clj-xpath "1.4.11"]
                  [org.jsoup/jsoup "1.10.2"]
                  [clojurewerkz/urly "1.0.0"]
                  [cheshire "5.7.1"]
                  [compojure "1.5.1"]
                  [manifold "0.1.6"]
                  [tulos/manifail "0.4.0"]
                  [aleph "0.4.3"]])
