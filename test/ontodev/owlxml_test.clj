(ns ontodev.owlxml-test
  (:use midje.sweet)
  (:require [ontodev.owlxml :as xml]))

;; ## Create a test directory
(.mkdir (clojure.java.io/file "target/test"))

;; ## Utilities

(defn obo [& strings] (apply str "http://purl.obolibrary.org/obo/" strings))
(defn ncbi [id] (obo "NCBITaxon_" id))

(defn owl-annotation [base id]
  (str "<owl:AnnotationProperty rdf:about=\"" base id "\">"))
(defn obo-annotation [id]
  (owl-annotation "http://purl.obolibrary.org/obo/" id))
(defn oio-annotation [id]
  (owl-annotation "http://www.geneontology.org/formats/oboInOwl#" id))


;; ## Extract Hierarchy

(fact "NCBI Taxonomy test hierarchy"
  (xml/extract-hierarchy "resources/ncbi_human.owl") =>
    (contains {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
               (ncbi 9605)   (ncbi 207598) ; Homo
               (ncbi 9443)   (ncbi 314146) ; Primate
               (ncbi 40674)  (ncbi 32524)  ; Mammal
               (ncbi 7742)   (ncbi 89593)  ; Vertebrate
               (ncbi 2759)   (obo "OBI_0100026") ; Eukaryote
               }))

(fact "NCBI Taxonomy test resources"
  (xml/extract-resources "resources/ncbi_human.owl") =>
    (contains {(ncbi 9606)   #{(ncbi 9605)}   ; Homo sapiens
               (ncbi 9605)   #{(ncbi 207598)} ; Homo
               (obo "OBI_0100026")
                 #{(ncbi 1) (ncbi 2) (ncbi 2157) (ncbi 2759) (ncbi 10239)} ; organism
               }))

(def ancestree {6 5, 5 4, 4 3, 3 2, 2 1, 7 3, 8 7, 9 6})

(fact "Check ancestry"
  (xml/ancestry ancestree 6) => [6 5 4 3 2 1])

(fact "Check extract-ancestry"
  (xml/extract-ancestor-closure ancestree #{6 7}) => #{1 2 3 4 5 6 7})

(fact "Check looping ancestry"
  (xml/extract-ancestor-closure {1 2, 2 1} 1) => (throws Exception))

(facts "Check longest initial subsequence"
  (xml/longest-initial-subsequence [1 2 3 4]) => [1 2 3 4]
  (xml/longest-initial-subsequence [1 2 3 4] []) => nil
  (xml/longest-initial-subsequence [1 2 3 4] [1 2 3]) => [1 2 3]
  (xml/longest-initial-subsequence [1 2 3 4] [7 8 9]) => nil
  (xml/longest-initial-subsequence [1 2 3 4] [1 2 3] []) => nil
  (xml/longest-initial-subsequence [1 2 3 4] [1 2 3] [7 8 9]) => nil
  (xml/longest-initial-subsequence [1 2 3 4] [1 2 3] [1 2]) => [1 2])

(def resources {1 #{2 3}, 2 #{4}, 4 #{2 5}}) 

(fact "Check extract-resource-closure"
  (xml/extract-resource-closure resources #{1}) => #{1 2 3 4 5})


;; ## Extract Classes

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test.owl"
                     "http://ontodev.com/ncbi_test"
                     #{(ncbi 9606) (ncbi 9605)})

(fact "Extracted test hierarchy"
  (xml/extract-hierarchy "target/test/ncbi_test.owl") =>
    (just {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
           (ncbi 9605)   (ncbi 207598) ; Homo
           }))

(let [text (slurp "target/test/ncbi_test.owl")]
  (fact "IRI for ncbi-eukaryote ontology is http://ontodev.com/ncbi_test"
    (.contains text "    <owl:Ontology rdf:about=\"http://ontodev.com/ncbi_test\"/>") => true)
  (tabular
    (fact "IRI in ncbi_test"
      (.contains text ?class) => ?value)
    ?value ?class                             ?name
    true   (obo-annotation "IAO_0000115")     "definition"
    true   (oio-annotation "hasExactSynonym") "hasExactSynonym"
    ))


;; ## Extract Classes Into

(xml/extract-classes-into "target/test/ncbi_test.owl"
                          "resources/ncbi_mouse.owl"
                          "target/test/ncbi_test2.owl"
                          "http://ontodev.com/ncbi_test2"
                          #{(ncbi 10090)})

(fact "Extracted test hierarchy 2"
  (xml/extract-hierarchy "target/test/ncbi_test2.owl") =>
    (just {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
           (ncbi 9605)   (ncbi 207598) ; Homo
           (ncbi 10090)  (ncbi 862507) ; Mouse
           }))

(let [text (slurp "target/test/ncbi_test2.owl")]
  (fact "IRI for ncbi-eukaryote ontology is http://ontodev.com/ncbi_test2"
    (.contains text "    <owl:Ontology rdf:about=\"http://ontodev.com/ncbi_test2\"/>") => true)
  (tabular
    (fact "IRI in ncbi_test2"
      (.contains text ?class) => ?value)
    ?value ?class                             ?name
    true   (obo-annotation "IAO_0000115")     "definition"
    true   (oio-annotation "hasExactSynonym") "hasExactSynonym"
    ))


;; ## Extract a Hierarchy of Classes

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test3.owl"
                     "http://ontodev.com/ncbi_test3"
                     (xml/extract-ancestor-closure
                       (xml/extract-hierarchy "resources/ncbi_human.owl")
                       #{(ncbi 9606)}))

(fact "Extract test hierarchy 3"
  (xml/extract-hierarchy "target/test/ncbi_test3.owl") =>
    (contains {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
               (ncbi 9605)   (ncbi 207598) ; Homo
               (ncbi 9443)   (ncbi 314146) ; Primate
               (ncbi 40674)  (ncbi 32524)  ; Mammal
               (ncbi 7742)   (ncbi 89593)  ; Vertebrate
               (ncbi 2759)   (obo "OBI_0100026") ; Eukaryote
               }))

;; ## Extract Classes by Resources

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test4.owl"
                     "http://ontodev.com/ncbi_test4"
                     (xml/extract-resource-closure
                       (xml/extract-resources "resources/ncbi_human.owl")
                       #{(obo "OBI_0100026")}))

(fact "Extract test hierarchy 4"
  (xml/extract-hierarchy "target/test/ncbi_test4.owl") =>
    (contains {(ncbi 2)            (ncbi 131567)       ; Bacteria
               (ncbi 2157)         (ncbi 131567)       ; Archaea
               (ncbi 2759)         (obo "OBI_0100026") ; Eukaryote
               (ncbi 10239)        (ncbi 1)            ; Viruses
               (ncbi 131567)       (ncbi 1)            ; cellular organism
               (obo "OBI_0100026") nil                 ; organism
               }))
