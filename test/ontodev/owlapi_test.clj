(ns ontodev.owlapi-test
  (:use midje.sweet)
  (:require [clojure.java.io :as io]
            [ontodev.owlapi :as owl])
  (:import (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI)))

(def human-path "resources/ncbi_human.owl")
(def mouse-path "resources/ncbi_mouse.owl")
(def human "http://purl.obolibrary.org/obo/NCBITaxon_9606")
(def mouse "http://purl.obolibrary.org/obo/NCBITaxon_10090")
(def organism "http://purl.obolibrary.org/obo/OBI_0100026")
(def hasExactSynonym "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
(def hasSynonymType "http://www.geneontology.org/formats/oboInOwl#hasSynonymType")
(def scientificName "http://purl.obolibrary.org/obo/ncbitaxon#scientific_name")
(def testProperty "http://foo.bar/testProperty")

(let [ontology (owl/load-ontology human-path)]
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
    (fact (owl/shorten (.getOWLClass owl/data-factory (IRI/create organism))) =>
          organism)
    (fact (owl/in-namespace? "ncbi" "ncbi:9606") => true)
    (fact (owl/in-namespace? "ncbi:" "ncbi:9606") => true)
    (fact (owl/in-namespace? "ncb:" "ncbi:9606") => false)
    (fact (owl/in-namespace? "ncbi" "iedb:9606") => false)

    ; Work with literal annotations
    (fact (owl/annotations ontology human testProperty) => [])
    (fact (count (owl/annotation-axioms ontology human)) => 7)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 0)
    (owl/annotate! ontology human testProperty "FOO")
    (fact (owl/annotations ontology human testProperty) => ["FOO"])
    (fact (count (owl/annotation-axioms ontology human)) => 8)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 1)
    (owl/remove-annotations! ontology human testProperty)
    (fact (owl/annotations ontology human testProperty) => [])
    (fact (count (owl/annotation-axioms ontology human)) => 7)
    (fact (count (owl/annotation-axioms ontology human testProperty)) => 0)

    ; Work with resource annotations
    (owl/annotate! ontology human testProperty (owl/expand "http://foo"))
    (fact (owl/annotations ontology human testProperty) => ["http://foo"])
    (owl/remove-annotations! ontology human testProperty)

    ; Annotated annotations
    (fact (count (owl/annotations+ ontology human)) => 7)
    (fact (count (owl/annotations+ ontology human testProperty)) => 0)
    (owl/annotate+! ontology human
                    testProperty (owl/expand "http://foo")
                    "rdfs:comment" "FOO")
    (fact (count (owl/annotations+ ontology human testProperty)) => 1)
    (fact (owl/annotations+ ontology human testProperty) =>
          [["http://foo" [["rdfs:comment" "FOO"]]]])
    (fact (owl/annotations+ ontology human testProperty "http://foo") =>
          [["rdfs:comment" "FOO"]])
    (fact (owl/annotation+ ontology human testProperty) =>
          ["http://foo" [["rdfs:comment" "FOO"]]])
    (fact (owl/annotation+ ontology human testProperty "http://foo") =>
          ["rdfs:comment" "FOO"])
    (owl/remove-annotations! ontology human testProperty)

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

    ; Replace the labels using annotation
    (owl/relabel! ontology human "Homo sapiens"
                  hasSynonymType (owl/expand scientificName))
    (fact (owl/label ontology human) => "Homo sapiens")
    (fact (owl/labels ontology human) => ["Homo sapiens"])
    (fact (owl/annotations ontology human hasExactSynonym) =>
          (contains ["human" "man"] :in-any-order))
    (fact (owl/annotations+ ontology human "rdfs:label") =>
          (contains [["Homo sapiens" [[hasSynonymType scientificName]]]]))
    (fact (owl/annotations+ ontology human hasExactSynonym) =>
          (contains [["man"   []]
                     ["human" [["rdfs:comment" "Xoiyaeuod"]]]]
                    :in-any-order))

    ; Replace label, reassigning existing labels to synonyms
    (owl/relabel! ontology human "Human" hasExactSynonym)
    (fact (owl/label ontology human) => "Human")
    (fact (owl/labels ontology human) => ["Human"])
    (fact (owl/annotations ontology human hasExactSynonym) =>
          (contains ["Homo sapiens" "human" "man"] :in-any-order))

    ; Replace label, demoting existing labels to synonyms with annotations
    (owl/relabel! ontology human "Human"
                  hasExactSynonym hasSynonymType (owl/expand scientificName))
    (fact (owl/label ontology human) => "Human")
    (fact (owl/labels ontology human) => ["Human"])
    (fact (owl/annotations+ ontology human "rdfs:label") =>
          (contains [["Human" []]]))
    (fact (owl/annotations+ ontology human hasExactSynonym) =>
          (contains [["Homo sapiens" []]
                     ["Human"        [[hasSynonymType scientificName]]]
                     ["man"          []]
                     ["human"        [["rdfs:comment" "Xoiyaeuod"]]]]
                    :in-any-order))

    ; Replace label with annotation,
    ; reassigning existing labels to synonyms with annotations
    (owl/relabel! ontology human "Human"
                  hasSynonymType (owl/expand scientificName)
                  hasExactSynonym hasSynonymType (owl/expand scientificName))
    (fact (owl/label ontology human) => "Human")
    (fact (owl/labels ontology human) => ["Human"])
    (fact (owl/annotations+ ontology human "rdfs:label") =>
          (contains [["Human" [[hasSynonymType scientificName]]]]))
    (fact (owl/annotations+ ontology human hasExactSynonym) =>
          (contains [["Homo sapiens" []]
                     ["Human"        [[hasSynonymType scientificName]]]
                     ["man"          []]
                     ["human"        [["rdfs:comment" "Xoiyaeuod"]]]]
                    :in-any-order))

    ; Test ancestry
    (fact "human is a primate"
      (some #(= "ncbi:9443" %) (owl/ancestry ontology "ncbi:9606")) => true)
    (fact "ancestry of 33213"
      (owl/ancestry ontology "ncbi:33213") =>
      ["ncbi:33213" "ncbi:6072" "ncbi:33208" "ncbi:33154" "ncbi:2759" "http://purl.obolibrary.org/obo/OBI_0100026"])
    (fact "ancestry of 9606"
      (owl/ancestry ontology "ncbi:9606") =>
      ["ncbi:9606" "ncbi:9605" "ncbi:207598" "ncbi:9604" "ncbi:314295" "ncbi:9526" "ncbi:314293" "ncbi:376913" "ncbi:9443" "ncbi:314146" "ncbi:9347" "ncbi:32525" "ncbi:40674" "ncbi:32524" "ncbi:32523" "ncbi:8287" "ncbi:117571" "ncbi:117570" "ncbi:7776" "ncbi:7742" "ncbi:89593" "ncbi:7711" "ncbi:33511" "ncbi:33213" "ncbi:6072" "ncbi:33208" "ncbi:33154" "ncbi:2759" "http://purl.obolibrary.org/obo/OBI_0100026"])

    #_(owl/save-ontology ontology "test.owl")

    (finally (owl/remove-ontology ontology))))

(owl/merge-ontologies "http://merged.org" "merged.owl" human-path mouse-path)
(let [ontology (owl/load-ontology "merged.owl")]
  (try
    (fact "human is a primate"
      (some #(= "ncbi:9443" %) (owl/ancestry ontology "ncbi:9606")) => true)
    (fact "mouse is a rodent"
      (some #(= "ncbi:9989" %) (owl/ancestry ontology "ncbi:10090")) => true)

    (finally (owl/remove-ontology ontology)
             (io/delete-file "merged.owl"))))
