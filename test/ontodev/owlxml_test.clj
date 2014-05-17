(ns ontodev.owlxml-test
  (:require [clojure.test :refer :all]
            [ontodev.owlxml :as xml]))

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

(def some-human-ancestors
  {(ncbi 9606) (ncbi 9605)   ; Homo sapiens
   (ncbi 9605) (ncbi 207598) ; Homo
   (ncbi 9443) (ncbi 314146) ; Primate
   (ncbi 40674)(ncbi 32524)  ; Mammal
   (ncbi 7742) (ncbi 89593)  ; Vertebrate
   (ncbi 2759) (obo "OBI_0100026") ; Eukaryote
   })

(deftest test-ncbi-hierarchy
  (is (= (select-keys (xml/extract-hierarchy "resources/ncbi_human.owl")
                      (keys some-human-ancestors))
         some-human-ancestors)))

(def some-resources
  {(ncbi 9606)   #{(ncbi 9605)}   ; Homo sapiens
   (ncbi 9605)   #{(ncbi 207598)} ; Homo
   (obo "OBI_0100026")
   #{(ncbi 1) (ncbi 2) (ncbi 2157) (ncbi 2759) (ncbi 10239)} ; organism
   })

(deftest test-ncbi-resources
  (is (= (select-keys (xml/extract-resources "resources/ncbi_human.owl")
                      (keys some-resources))
         some-resources)))

(def ancestree {6 5, 5 4, 4 3, 3 2, 2 1, 7 3, 8 7, 9 6})

(deftest test-ancestry
  (is (= (xml/ancestry ancestree 6)
         [6 5 4 3 2 1])))

(deftest test-extract-ancestry
  (is (= (xml/extract-ancestor-closure ancestree #{6 7})
         #{1 2 3 4 5 6 7})))

(deftest test-ancestry-loop
  (is (thrown? Exception (xml/extract-ancestor-closure {1 2, 2 1} 1))))

(deftest test-longest-initial-subsequence
  (are [xs y] (= (apply xml/longest-initial-subsequence xs) y)
       [[1 2 3 4]]
       [1 2 3 4]
       [[1 2 3 4] []]
       nil
       [[1 2 3 4] [1 2 3]]
       [1 2 3]
       [[1 2 3 4] [7 8 9]]
       nil
       [[1 2 3 4] [1 2 3] []]
       nil
       [[1 2 3 4] [1 2 3] [7 8 9]]
       nil
       [[1 2 3 4] [1 2 3] [1 2]]
       [1 2]))

(def resources {1 #{2 3}, 2 #{4}, 4 #{2 5}})

(deftest text-extract-resource-closure
  (is (= (xml/extract-resource-closure resources #{1}) #{1 2 3 4 5})))


;; ## Extract Classes

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test.owl"
                     "http://ontodev.com/ncbi_test"
                     #{(ncbi 9606) (ncbi 9605)})

(deftest test-extract-hierarchy
  (is (= (xml/extract-hierarchy "target/test/ncbi_test.owl")
         {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
          (ncbi 9605)   (ncbi 207598) ; Homo
          })))

(deftest test-extracted-tags
  (let [text (slurp "target/test/ncbi_test.owl")]
    (is (.contains text "    <owl:Ontology rdf:about=\"http://ontodev.com/ncbi_test\"/>"))
    (are [x] (.contains text x)
         (obo-annotation "IAO_0000115")
         (oio-annotation "hasExactSynonym"))))


;; ## Extract Classes Into

(xml/extract-classes-into "target/test/ncbi_test.owl"
                          "resources/ncbi_mouse.owl"
                          "target/test/ncbi_test2.owl"
                          "http://ontodev.com/ncbi_test2"
                          #{(ncbi 10090)})

(deftest test-extract-hierarchy-2
  (is (= (xml/extract-hierarchy "target/test/ncbi_test.owl")
         {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
          (ncbi 9605)   (ncbi 207598) ; Homo
          })))

(deftest test-extracted-tags-2
  (let [text (slurp "target/test/ncbi_test2.owl")]
    (is (.contains text "    <owl:Ontology rdf:about=\"http://ontodev.com/ncbi_test2\"/>"))
    (are [x] (.contains text x)
         (obo-annotation "IAO_0000115")
         (oio-annotation "hasExactSynonym"))))


;; ## Extract a Hierarchy of Classes

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test3.owl"
                     "http://ontodev.com/ncbi_test3"
                     (xml/extract-ancestor-closure
                       (xml/extract-hierarchy "resources/ncbi_human.owl")
                       #{(ncbi 9606)}))

(def test-hierarchy-3
  {(ncbi 9606)   (ncbi 9605)   ; Homo sapiens
   (ncbi 9605)   (ncbi 207598) ; Homo
   (ncbi 9443)   (ncbi 314146) ; Primate
   (ncbi 40674)  (ncbi 32524)  ; Mammal
   (ncbi 7742)   (ncbi 89593)  ; Vertebrate
   (ncbi 2759)   (obo "OBI_0100026") ; Eukaryote
   })

(deftest test-extract-hierarchy-3
  (is (= (select-keys (xml/extract-hierarchy "target/test/ncbi_test3.owl")
                      (keys test-hierarchy-3))
         test-hierarchy-3)))


;; ## Extract Classes by Resources

(xml/extract-classes "resources/ncbi_human.owl"
                     "target/test/ncbi_test4.owl"
                     "http://ontodev.com/ncbi_test4"
                     (xml/extract-resource-closure
                       (xml/extract-resources "resources/ncbi_human.owl")
                       #{(obo "OBI_0100026")}))

(def test-hierarchy-4
  {(ncbi 2)            (ncbi 131567)       ; Bacteria
   (ncbi 2157)         (ncbi 131567)       ; Archaea
   (ncbi 2759)         (obo "OBI_0100026") ; Eukaryote
   (ncbi 10239)        (ncbi 1)            ; Viruses
   (ncbi 131567)       (ncbi 1)            ; cellular organism
   (obo "OBI_0100026") nil                 ; organism
   })

(deftest test-extract-hierarchy-4
  (is (= (select-keys (xml/extract-hierarchy "target/test/ncbi_test4.owl")
                      (keys test-hierarchy-4))
         test-hierarchy-4)))
