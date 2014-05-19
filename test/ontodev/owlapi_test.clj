(ns ontodev.owlapi-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [ontodev.owlapi :as owl])
  (:import (org.semanticweb.owlapi.model IRI)))

(def human-path "resources/ncbi_human.owl")
(def mouse-path "resources/ncbi_mouse.owl")
(def human "http://purl.obolibrary.org/obo/NCBITaxon_9606")
(def mouse "http://purl.obolibrary.org/obo/NCBITaxon_10090")
(def organism "http://purl.obolibrary.org/obo/OBI_0100026")
(def hasExactSynonym "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
(def hasSynonymType "http://www.geneontology.org/formats/oboInOwl#hasSynonymType")
(def scientificName "http://purl.obolibrary.org/obo/ncbitaxon#scientific_name")
(def testProperty "http://foo.bar/testProperty")

(defn unordered
  "Order is undefined for many OWLAPI results.
   This function just returns its input, except when the input is
   sequential, in which case it returns the sorted sequence for easier
   comparison."
  [input]
  (if (sequential? input)
    (->> input
         (clojure.walk/postwalk #(if (sequential? %) (vec %) %))
         sort)
    input))

(deftest test-catalog
  (is (= (owl/parse-catalog (io/file "resources/catalog.xml"))
         {(IRI/create "http://from.some/file.owl")
          (IRI/create (io/file "resources/target.owl"))
          (IRI/create "duplicate:http://purl.obolibrary.org/obo/your_ontology/external/NCBITaxon_import.owl")
          (IRI/create "file:/Users/james/Documents/Development/Repositories/ontodev/owlapi/resources/ncbi_mouse.owl")})))

(deftest test-literal
  (are [x] (= (owl/literal? x) true)
       "string"
       true
       100
       100.00)
  (are [x] (= (owl/literal? x) false)
       str))

(deftest test-ontology
  (let [ontology (owl/load-ontology human-path)]
    (try
      (testing "Chech that ontology is loaded"
        (is ontology))

      (testing "Expand and shorten"
        (owl/add-prefix "ncbi" "http://purl.obolibrary.org/obo/NCBITaxon_")
        (owl/add-prefix "iedb" "http://iedb.org/IEDBTaxon_")
        (are [x] (= x (IRI/create human))
             (owl/expand "ncbi:9606")
             (owl/expand (.getOWLClass owl/data-factory (IRI/create human))))
        (are [x] (= x "ncbi:9606")
             (owl/shorten "ncbi:9606")
             (owl/shorten "http://purl.obolibrary.org/obo/NCBITaxon_9606")
             (owl/shorten (.getOWLClass owl/data-factory (IRI/create human))))
        (is (= (owl/shorten (.getOWLClass owl/data-factory (IRI/create organism)))
                organism))
        (are [s n] (= (owl/in-namespace? s n) true)
             "ncbi"  "ncbi:9606"
             "ncbi:" "ncbi:9606")
        (are [s n] (= (owl/in-namespace? s n) false)
             "ncb:" "ncbi:9606"
             "ncbi" "iedb:9606"))

      (testing "Test literals"
        (are [x y] (= x y)
             (owl/annotations ontology human testProperty) []
             (owl/annotations ontology human [testProperty]) []
             (count (owl/annotation-axioms ontology human)) 7
             (count (owl/annotation-axioms ontology human testProperty)) 0
             (count (owl/annotation-axioms ontology human [testProperty])) 0)
        (owl/annotate! ontology human testProperty 100)
        (are [x y] (= x y)
             (owl/annotations ontology human testProperty) ["100"]
             (owl/annotations ontology human [testProperty]) ["100"]
             (count (owl/annotation-axioms ontology human)) 8
             (count (owl/annotation-axioms ontology human testProperty)) 1
             (count (owl/annotation-axioms ontology human [testProperty])) 1)
        (owl/remove-annotations! ontology human testProperty)
        (are [x y] (= x y)
             (owl/annotations ontology human testProperty) []
             (count (owl/annotation-axioms ontology human)) 7
             (count (owl/annotation-axioms ontology human testProperty)) 0))

      (testing "Test resource annotations"
        (owl/annotate! ontology human testProperty (owl/expand "http://foo"))
        (is (= (owl/annotations ontology human testProperty) ["http://foo"]))
        (owl/remove-annotations! ontology human testProperty))

      (testing "Test annotated annotations"
        (are [x y] (= x y)
             (count (owl/annotations+ ontology human)) 7
             (count (owl/annotations+ ontology human testProperty)) 0)
        (owl/annotate+! ontology human
                        testProperty (owl/expand "http://foo")
                        "rdfs:comment" "FOO")
        (are [x y] (= x y)
             (count (owl/annotations+ ontology human testProperty))
             1
             (owl/annotations+ ontology human testProperty)
             [["http://foo" [["rdfs:comment" "FOO"]]]]
             (owl/annotations+ ontology human testProperty "http://foo")
             [["rdfs:comment" "FOO"]]
             (owl/annotation+ ontology human testProperty)
             ["http://foo" [["rdfs:comment" "FOO"]]]
             (owl/annotation+ ontology human [testProperty])
             ["http://foo" [["rdfs:comment" "FOO"]]]
             (owl/annotation+ ontology human testProperty "http://foo")
             ["rdfs:comment" "FOO"])
        (owl/remove-annotations! ontology human testProperty))

      (testing "Check current labels"
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Homo sapiens"
             (owl/labels ontology human)
             ["Homo sapiens"]
             (owl/annotations ontology human hasExactSynonym)
             ["human" "man"]))

      (testing "Add label"
        (owl/label! ontology human "Human")
        (is (= (unordered (owl/labels ontology human))
               ["Homo sapiens" "Human"])))

      (testing "Relabel"
        (owl/relabel! ontology human "Homo sapiens")
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Homo sapiens"
             (owl/labels ontology human)
             ["Homo sapiens"]
             (owl/annotations ontology human hasExactSynonym)
             ["human" "man"]))

      (testing "Relabel with annotation"
        (owl/relabel! ontology human "Homo sapiens"
                      hasSynonymType (owl/expand scientificName))
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Homo sapiens"
             (owl/labels ontology human)
             ["Homo sapiens"]
             (owl/annotations ontology human hasExactSynonym)
             ["human" "man"]
             (owl/annotations+ ontology human "rdfs:label")
             [["Homo sapiens" [[hasSynonymType scientificName]]]]
             (owl/annotations+ ontology human hasExactSynonym)
             [["human" [["rdfs:comment" "Xoiyaeuod"]]]
              ["man"   []]]))

      (testing "Relabel and reassign"
        (owl/relabel! ontology human "Human" hasExactSynonym)
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Human"
             (owl/labels ontology human)
             ["Human"]
             (owl/annotations ontology human hasExactSynonym)
             ["Homo sapiens" "human" "man"]))

      (testing "Relabel and reassign with annotation"
        (owl/relabel! ontology human "Human"
                      hasExactSynonym hasSynonymType (owl/expand scientificName))
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Human"
             (owl/labels ontology human)
             ["Human"]
             (owl/annotations ontology human hasExactSynonym)
             ["Homo sapiens" "Human" "human" "man"]
             (owl/annotations+ ontology human "rdfs:label")
             [["Human" []]]
             (owl/annotations+ ontology human hasExactSynonym)
             [["Homo sapiens" []]
              ["Human"        [[hasSynonymType scientificName]]]
              ["human"        [["rdfs:comment" "Xoiyaeuod"]]]
              ["man"          []]]))

      (testing "Replace label with annotation"
        (owl/relabel! ontology human "Human"
                      hasSynonymType (owl/expand scientificName)
                      hasExactSynonym hasSynonymType (owl/expand scientificName))
        (are [x y] (= (unordered x) y)
             (owl/label ontology human)
             "Human"
             (owl/labels ontology human)
             ["Human"]
             (owl/annotations ontology human hasExactSynonym)
             ["Homo sapiens" "Human" "human" "man"]
             (owl/annotations+ ontology human "rdfs:label")
             [["Human" [[hasSynonymType scientificName]]]]
             (owl/annotations+ ontology human hasExactSynonym)
             [["Homo sapiens" []]
              ["Human"        [[hasSynonymType scientificName]]]
              ["human"        [["rdfs:comment" "Xoiyaeuod"]]]
              ["man"          []]]))

      (testing "Test ancestry"
        (is (some #(= "ncbi:9443" %) (owl/ancestry ontology "ncbi:9606")))
        (is (= (owl/ancestry ontology "ncbi:33213")
              ["ncbi:33213" "ncbi:6072" "ncbi:33208" "ncbi:33154" "ncbi:2759" "ncbi:131567" "ncbi:1"]))
        (is (= (owl/ancestry ontology "ncbi:9606")
              ["ncbi:9606" "ncbi:9605" "ncbi:207598" "ncbi:9604" "ncbi:314295" "ncbi:9526" "ncbi:314293" "ncbi:376913" "ncbi:9443" "ncbi:314146" "ncbi:9347" "ncbi:32525" "ncbi:40674" "ncbi:32524" "ncbi:32523" "ncbi:8287" "ncbi:117571" "ncbi:117570" "ncbi:7776" "ncbi:7742" "ncbi:89593" "ncbi:7711" "ncbi:33511" "ncbi:33213" "ncbi:6072" "ncbi:33208" "ncbi:33154" "ncbi:2759" "ncbi:131567" "ncbi:1"])))

      ; Add and then remove a new class
      (testing "add and remove classes"
        (is (= (owl/label ontology "ncbi:foo") nil))
        (owl/add-class! ontology "ncbi:foo" "NCBI Foo")
        (is (= (owl/label ontology "ncbi:foo") "NCBI Foo"))
        (owl/remove-class! ontology "ncbi:foo")
        (is (= (owl/label ontology "ncbi:foo") nil)))

      ; Make sure reasoner runs
      (let [reasoner (owl/reasoner ontology)]
        (owl/reason! reasoner ontology)
        (owl/dispose! reasoner))

      #_(owl/save-ontology ontology "test.owl")

      (finally (owl/remove-ontology ontology)))))

(deftest test-merge
  (owl/merge-ontologies "http://merged.org" "merged.owl" human-path mouse-path)
  (let [ontology (owl/load-ontology "merged.owl")]
    (try
      (testing "human is a primate"
        (is (some #(= "ncbi:9443" %) (owl/ancestry ontology "ncbi:9606"))))
      (testing "mouse is a rodent"
        (is (some #(= "ncbi:9989" %) (owl/ancestry ontology "ncbi:10090"))))
      (finally (owl/remove-ontology ontology)
               (io/delete-file "merged.owl")))))

(deftest test-extract
  (let [ontology  (owl/load-ontology human-path)
        targets   #{"http://purl.obolibrary.org/obo/OBI_0100026"}
        extracted (owl/extract ontology targets "http://new.org")]
    (try
      (owl/save-ontology extracted "target/test/extracted.owl")
      (testing "check extracted class CURIEs"
        (is (= (set (owl/classes extracted))
               #{"ncbi:2759" "ncbi:2"
                 "http://purl.obolibrary.org/obo/OBI_0100026"
                 "ncbi:10239" "ncbi:2157"})))
      (finally (owl/remove-ontology ontology)
               (owl/remove-ontology extracted)))))

(deftest test-parse-and-render
  (let [ontology   (owl/load-ontology human-path)
        checker    (owl/entity-checker ontology)
        expression "Viruses and 'Homo sapiens'"]
    (try
      (is (= (owl/parse-class-expression checker expression)
             (owl/intersection ["ncbi:10239" "ncbi:9606"])))
      (finally (owl/remove-ontology ontology)))))

