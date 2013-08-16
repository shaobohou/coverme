(defproject coverme "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :min-lein-version "2.0.0"

  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-http "0.7.6"]
                 [cheshire "5.2.0"]
                 [compojure "1.1.5"]]

  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler coverme.core/app}

  :main coverme.core)
