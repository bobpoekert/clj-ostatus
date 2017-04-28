(defproject ostatus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [diff-eq "0.2.3"]]
                   :injections [(require 'diff-eq.core)
                                (diff-eq.core/diff!)]}}
  :dependencies [
                  [org.clojure/clojure "1.9.0-alpha15"]
                  [com.gfredericks/test.chuck "0.2.7"]
                  [com.github.kyleburton/clj-xpath "1.4.11"]
                  [org.jsoup/jsoup "1.10.2"]
                  [hiccup "1.0.5"]
                  [compojure "1.5.1"]
                  [manifold "0.1.6"]])