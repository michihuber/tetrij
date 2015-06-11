(set-env!
  :source-paths      #{"src" "test"}
  :resource-paths    #{"html"}
  :dependencies '[[adzerk/boot-cljs "0.0-2629-1"]
                  [adzerk/boot-cljs-repl  "0.1.7"]
                  [adzerk/boot-reload     "0.2.3"]
                  [pandeiro/boot-http "0.4.2"]

                  [reagent "0.5.0-alpha"]
                  [org.clojure/core.match "0.3.0-alpha4"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                  [com.cemerick/clojurescript.test "0.3.3"]])

(require
  '[pandeiro.boot-http         :refer [serve]]
  '[adzerk.boot-cljs      :refer [cljs]]
  '[adzerk.boot-cljs-repl :refer :all]
  '[adzerk.boot-reload    :refer [reload]])

(deftask dev
  "Build cljs example for development."
  []
  (comp (serve :dir "target/" :port 5000)
        (watch)
        (speak)
        (reload)
        (cljs-repl)
        (cljs :unified-mode true
              :pretty-print true
              :source-map true
              :optimizations :none)
        (show)))

