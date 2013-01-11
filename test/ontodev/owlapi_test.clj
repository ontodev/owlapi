(ns ontodev.owlapi-test
  (:use midje.sweet)
  (:require [ontodev.owlapi :as owl])
  (:import (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI)))

(def ontology-path "resources/ncbi_human.owl")
(def human "http://purl.obolibrary.org/obo/NCBITaxon_9606")
(def hasExactSynonym "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
(def testProperty "http://foo.bar/testProperty")

(let [ontology (owl/load-ontology ontology-path)]
  (try
    (fact ontology => truthy)
    ;(fact (owl/annotations ontology human) => "")

    ; Check expand and shorten
    (owl/add-prefix "ncbi" "http://purl.obolibrary.org/obo/NCBITaxon_")
    (owl/add-prefix "iedb" "http://iedb.org/IEDBTaxon_")
    (fact (owl/expand "ncbi:9606") => (IRI/create human))
    (fact (owl/expand (.getOWLClass owl/data-factory (IRI/create human))) =>
          (IRI/create human))
    (fact (owl/shorten "ncbi:9606") => "ncbi:9606")
    (fact (owl/shorten "http://purl.obolibrary.org/obo/NCBITaxon_9606") =>
          "ncbi:9606")
    (fact (owl/shorten (.getOWLClass owl/data-factory (IRI/create human))) =>
          "ncbi:9606")
    (fact (owl/in-namespace? "ncbi" "ncbi:9606") => true)
    (fact (owl/in-namespace? "ncbi:" "ncbi:9606") => true)
    (fact (owl/in-namespace? "ncb:" "ncbi:9606") => false)
    (fact (owl/in-namespace? "ncbi" "iedb:9606") => false)
    
    ; Check current labels and synonyms
    (fact (owl/label ontology human) => "Homo sapiens")
    (fact (owl/labels ontology human) => ["Homo sapiens"])
    (fact (owl/annotations ontology human hasExactSynonym) =>
          (contains ["human" "man"] :in-any-order))

    ; Add a label
    (owl/label! ontology human "Human")
    (fact (owl/labels ontology human) => ["Human" "Homo sapiens"])

    ; Replace the labels
    (owl/relabel! ontology human "Homo sapiens")
    (fact (owl/label ontology human) => "Homo sapiens")
    (fact (owl/labels ontology human) => ["Homo sapiens"])
    (fact (owl/annotations ontology human hasExactSynonym) =>
          (contains ["human" "man"] :in-any-order))

    ; Replace label, demoting existing labels to synonyms
    (owl/relabel! ontology human hasExactSynonym "Human")
    (fact (owl/label ontology human) => "Human")
    (fact (owl/labels ontology human) => ["Human"])
    (fact (owl/annotations ontology human hasExactSynonym) =>
          (contains ["Homo sapiens" "human" "man"] :in-any-order))

    ; Work with literal annotation
    (fact (owl/annotations ontology human testProperty) => [])
    (fact (count (owl/annotation-axioms ontology human)) => 9)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 0)
    (owl/annotate! ontology human testProperty "FOO")
    (fact (owl/annotations ontology human testProperty) => ["FOO"])
    (fact (count (owl/annotation-axioms ontology human)) => 10)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 1)
    (owl/remove-annotations! ontology human testProperty)
    (fact (owl/annotations ontology human testProperty) => [])
    (fact (count (owl/annotation-axioms ontology human)) => 9)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 0)
    
    ; Work with resource annotation
    (owl/annotate! ontology human testProperty (owl/expand "http://foo"))
    (fact (owl/annotations ontology human testProperty) => ["http://foo"])

    #_(owl/save-ontology ontology "test.owl")
    (finally (owl/remove-ontology ontology))))

