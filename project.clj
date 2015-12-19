(defproject culture-war-cljs "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/core.async "0.2.374"]
                 [org.omcljs/om "0.9.0"]]

  :plugins [[lein-cljsbuild "1.1.1"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["out" "out-adv"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :compiler {:main culture-war-cljs.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/cw_cljs.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}
               ;; This next build is an compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once min
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/cw_cljs.js"
                           :main culture-war-cljs.core
                           :optimizations :advanced
                           :pretty-print false}}]})
