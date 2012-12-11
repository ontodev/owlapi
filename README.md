# ontodev/owlapi

A thin Clojure wrapper around OWLAPI. 


## Usage

Add `ontodev/owlapi` to your [Leiningen](http://leiningen.org/) project dependencies:

    [ontodev/owlapi "0.1.0"]

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

See the documentation in `docs/uberdoc.html` for more information.


## License

Copyright © 2012, James A. Overton

Distributed under the Simplified BSD License: [http://opensource.org/licenses/BSD-2-Clause]()
