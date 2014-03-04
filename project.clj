(defproject ontodev/owlapi "0.3.0"
  :description "A thin Clojure wrapper around OWLAPI."
  :url "http://github.com/ontodev/owlapi"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.4"]
                 [net.sourceforge.owlapi/owlapi-api "3.4.10"]
                 [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.4"]
                 [org.codehaus.woodstox/woodstox-core-asl "4.1.4"]]
  :profiles
  {:dev {:dependencies [[midje "1.4.0"]
                        [lazytest "1.2.3"]]
         :plugins [[lein-midje "2.0.1"]
                   [lein-marginalia "0.7.1"]]}})
