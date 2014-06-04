(defproject ontodev/owlapi "0.3.2"
  :description "A thin Clojure wrapper around OWLAPI."
  :url "http://github.com/ontodev/owlapi"
  :license {:name "Simplified BSD License"
            :url "http://opensource.org/licenses/BSD-2-Clause"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [net.sourceforge.owlapi/owlapi-api "3.5.0"]
                 [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.4"]
                 [org.codehaus.woodstox/woodstox-core-asl "4.3.0"]
                 [org.clojure/data.zip "0.1.1"]]
  :profiles
  {:dev {:plugins [[lein-marginalia "0.7.1"]]}})
