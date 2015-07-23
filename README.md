# ontodev/owlapi

A thin Clojure wrapper around OWLAPI, and some utilities for working with RDF/XML representations of OWL ontologies.


## Usage

Add `ontodev/owlapi` to your [Leiningen](http://leiningen.org/) project dependencies:

    [ontodev/owlapi "0.3.2"]

### OWLAPI

Then `require` the namespace:

    (ns your.project
      (:require [ontodev.owlapi :as owl]))

Add a global prefix, load an ontology, add a class (under a parent, with a label), annotate it with an `rdfs:comment`, and save the ontology:

    (owl/add-prefix "ncbi" "http://purl.obolibrary.org/obo/NCBITaxon_")
    (let [ontology (owl/load-ontology "resources/ncbi_human.owl")]
      (owl/add-class! ontology "ncbi:10000000" "ncbi:9606"
                     "Homo sapiens ontologicus")
      (owl/annotate! ontology "ncbi:10000000" "rdfs:comment"
                     "A newly discovered subspecies of human.")
      (owl/save-ontology ontology "ncbi_humans.owl"))

### XML

You can also work with the lower-level RDF/XML representations of OWL ontologies. This can be faster if the ontology is very large and the operations are fairly basic. 

First `require` the namespace, then extract all the classes that are referred to in the definition of OBI "organisms" (recursively):

    (ns your.project
      (:require [ontodev.owlxml :as xml]))

    (xml/extract-classes
      "resources/ncbi_human.owl"
      "organism.owl"
      "http://ontodev.com/organism"
      (xml/extract-resource-closure
        (xml/extract-resources "resources/ncbi_human.owl")
        #{"http://purl.obolibrary.org/obo/OBI_0100026"}))

The arguments to `extract-classes` are:

1. the path to the input OWL file (in RDF/XML format!)
2. the path to the output OWL file
3. the IRI for the output OWL ontology
4. a set of all the class IRIs to extract

Item 4 is the "closure" obtained by using `extract-resources` to get a map from class IRIs to sets of IRIs referred to in that class, then starting with the entry for OBI "organism" and recursively collecting all the IRIs found.

In addition to the extracted classes, all the object properties and annotation properties in the input OWL file are copied to the output OWL file.


See the documentation in `docs/uberdoc.html` for more information.


## License

Copyright © 2014, James A. Overton

Distributed under the Simplified BSD License: [http://opensource.org/licenses/BSD-2-Clause]()
