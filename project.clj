(defproject noc-clojurescript "0.1.0-SNAPSHOT"
  :description "Implementation of samples from The Nature of Code in ClojureScript"
  :url "http://natureofcode.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.2.5"]
                 [org.clojure/clojurescript "0.0-2740"]]

  :plugins [[lein-cljsbuild "1.0.4"]]
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds [{:source-paths ["src"]
             :compiler
             {:output-to "js/main.js"
              :output-dir "out"
              :main "nature-of-code.forces.fluidresistance.core"
              :optimizations :advanced
              :pretty-print true}}]})
