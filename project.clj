(defproject ontodev/owlapi "0.3.0-SNAPSHOT"
  :description "A thin Clojure wrapper around OWLAPI."
  :url "http://github.com/ontodev/owlapi"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.4"]
                 [owlapi/owlapi "3.2.4"]
                 [org.semanticweb/HermiT "1.3.6" :exclusions [owlapi/owlapi]]
                 [org.codehaus.woodstox/woodstox-core-asl "4.1.4"]]
  :repositories [["bbop" "http://code.berkeleybop.org/maven/repository/"]]
  :profiles
  {:dev {:dependencies [[midje "1.4.0"]
                        [lazytest "1.2.3"]]
         :plugins [[lein-midje "2.0.1"]
                   [lein-marginalia "0.7.1"]]}})
