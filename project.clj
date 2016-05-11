(defproject foreclojure "0.1.0-SNAPSHOT"
  :description "Testbed for my solutions to 4clojure.com"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.8.51"]]
  :plugins [[lein-figwheel "0.5.1"]]
  :source-paths ["src" "src-cljs"]
  :profiles {:dev {:dependencies [[figwheel-sidecar "0.5.2"]
                                  [com.cemerick/piggieback "0.2.1"]]}}
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :cljsbuild {
              :builds [ {:id "dev"
                         :source-paths ["src-cljs"]
                         :figwheel true
                         :compiler {:main "foreclojure.core"
                                    :asset-path "js/out"
                                    :output-to "resources/public/js/foreclojure.js"
                                    :output-dir "resources/public/js/out" } } ]
              })
