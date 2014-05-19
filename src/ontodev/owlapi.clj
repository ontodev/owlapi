;; # OWLAPI Wrapper
;; This code provides a thin wrapper over the OWLAPI:
;; http://owlapi.sourceforge.net
;;
;; Unlike native Clojure data structures, OWLOntology is mutable. Following
;; Clojure naming conventions, functions that mutate data have names that
;; end with a bang "!". These functions usually return the OWL axiom that
;; was added to the ontology. Functions that just read from the ontology
;; usually return CURIE strings or lists of CURIE strings. Most of the functions
;; take an ontology and a CURIE string as arguments, resolve the CURIE to an
;; OWL entity, and then perform their function.
;;
;; We import a large number of classes from OWLAPI. We use the HermiT reasoner,
;; which only works with an older version of the OWLAPI, but is the default
;; reasoner for Protégé.
(ns ontodev.owlapi
  (:refer-clojure :exclude [class class? parents ancestors descendants labels
                            union complement intersection some])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :refer [xml-> xml1-> attr text]])
  (:import
    (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                  OWLClassExpression OWLClass OWLAnnotation
                                  OWLAxiom OWLAnnotationAssertionAxiom
                                  OWLNamedObject OWLLiteral OWLObjectProperty
                                  AxiomType
                                  OWLOntologyIRIMapper)
    (org.semanticweb.owlapi.apibinding OWLManager)
    (org.semanticweb.owlapi.io RDFXMLOntologyFormat)
    (org.semanticweb.owlapi.util DefaultPrefixManager OWLEntityRemover)
    (org.semanticweb.owlapi.reasoner InferenceType)
    (org.semanticweb.owlapi.util InferredAxiomGenerator
                                 InferredSubClassAxiomGenerator
                                 InferredEquivalentClassAxiomGenerator
                                 InferredDisjointClassesAxiomGenerator
                                 InferredClassAssertionAxiomGenerator
                                 InferredOntologyGenerator
                                 OWLOntologyMerger)
    (org.semanticweb.HermiT Reasoner Reasoner$ReasonerFactory)  
    (uk.ac.manchester.cs.owlapi.modularity SyntacticLocalityModuleExtractor
                                           ModuleType)))


;; ## Globals
;; In the current implementation all ontologies share a common OWLDataFactory
;; and OWLOntologyManager.
(def reasoner-factory (Reasoner$ReasonerFactory.))
(def data-factory     (OWLManager/getOWLDataFactory))
(def prefixes (atom {"xml:"  "http://www.w3.org/XML/1998/namespace"
                     "xsd:"  "http://www.w3.org/2001/XMLSchema#"
                     "rdf:"  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     "rdfs:" "http://www.w3.org/2000/01/rdf-schema#"
                     "owl:"  "http://www.w3.org/2002/07/owl#"}))

(declare expand)

(defn manager
  "Given an ontolgy, return its OWLOntologyManager."
  [ontology]
  (.getOWLOntologyManager ontology))

;; ## IRI Mapper
;; Load catalog.xml files and create an IRI mapper.
;; See http://protegewiki.stanford.edu/wiki/How_Owl_2.0_Imports_Work
(defn parse-catalog-iri
  "Given a catalog file and one of its `uri` entries, return an absolute IRI.
   URIs are assumed to be file paths and are resolved relative to the
   catalog file."
  [catalog-file uri-string]
  (let [file (io/file uri-string)]
    (if (.isAbsolute file)
      (IRI/create file)
      (->> (str (.getParent (.getCanonicalFile catalog-file)) "/" uri-string)
           io/file
           (.getCanonicalFile)
           IRI/create))))

(defn parse-catalog
  "Parse an OWLAPI catalog.xml file and
   return a map from IRI to (absolute) IRI."
  [catalog-file]
  (->> (-> catalog-file xml/parse zip/xml-zip (xml-> :group :uri))
       (map first)
       (map :attrs)
       (map (juxt #(IRI/create (:name %))
                  #(parse-catalog-iri catalog-file (:uri %))))
       (into {})))

(defn catalog-xml-iri-mapper
  [path]
  (let [mappings (parse-catalog (io/file path))]
    (reify
      OWLOntologyIRIMapper
      (getDocumentIRI [_ iri] (get mappings iri)))))


;; ## Ontologies
;; Create, load, save, and remove ontologies.
(defn create-ontology
  [iri]
  (log/info "Creating ontology:" iri)
  (.createOntology (OWLManager/createOWLOntologyManager data-factory)
                   (IRI/create iri)))

(defn merge-loaded-ontologies
  "Given a manager with some loaded ontologies
   and an IRI for the new merged ontology,
   merge and return the new merged ontology."
  [manager iri]
  (log/info "Merging" (count (.getOntologies manager)) "ontologies")
  (.createMergedOntology (OWLOntologyMerger. manager) manager (expand iri)))

(defn load-ontology
  "Given a path to an ontology file, an optional path to a catalog file,
   and an optional `merge` boolean,
   load and return the ontology."
  ([ontology-path]
   (load-ontology ontology-path nil false))
  ([ontology-path catalog-path]
   (load-ontology ontology-path catalog-path false))
  ([ontology-path catalog-path merge-ontologies]
   (log/info "Loading ontology:" ontology-path)
   (let [manager (OWLManager/createOWLOntologyManager data-factory)]
     (when catalog-path
       (log/info "Using catalog:" catalog-path)
       (.addIRIMapper manager (catalog-xml-iri-mapper catalog-path)))
     (let [ontology (.loadOntologyFromOntologyDocument
                      manager
                      (io/file ontology-path))
           iri      (.getOntologyIRI (.getOntologyID ontology))]
       (if merge-ontologies
         (merge-loaded-ontologies manager iri)
         ontology)))))

(defn save-ontology
  "Save an ontology to a path and return the ontology"
  [ontology path]
  (log/info "Saving ontology:" path)
  (.saveOntology (manager ontology)
                 ontology
                 (IRI/create (io/as-url (io/file path))))
  ontology)

(defn remove-ontology
  "Remove an ontology from its manager."
  [ontology]
  (log/info "Removing ontology:" ontology)
  (.removeOntology (manager ontology) ontology))

(defn merge-ontologies
  "Load one or more ontologies from files, merge with a new IRI, and save to
   a new file, and return it."
  [iri-string save-path & merge-paths]
  (let [manager (OWLManager/createOWLOntologyManager data-factory)]
    (doseq [path merge-paths]
      (log/info "Loading ontology for merge:" path)
      (.loadOntologyFromOntologyDocument manager (io/file path)))
    (log/info "Creating ontology:" iri-string)
    (save-ontology (merge-loaded-ontologies manager iri-string) save-path)))


;; ## IRIs
;; It's more convenient to work with CURIE strings, but that means we have to
;; convert back and forth between strings, IRIs, and objects.

(defn append-colon
  "Append a colon to the string if required."
  [s] (if (.endsWith s ":") s (str s ":")))

(defn add-prefix
  "Add a prefix to the shared prefix manager."
  [prefix iri]
  (swap! prefixes assoc (append-colon prefix) iri))

(defn expand
  "Take a CURIE string, IRI, or OWLNamedObject and return an IRI."
  [input]
  (cond (instance? IRI input)
        input
        (instance? OWLNamedObject input)
        (.getIRI input)
        (and (string? input) (re-find #".+:" input))
        (let [prefix (re-find #".+:" input)
              result (get @prefixes prefix)]
          (cond (contains? #{"http:"} prefix)
                (IRI/create input)
                (and prefix result)
                (IRI/create (string/replace input prefix result))
                :else
                (IRI/create input)))
        :else
        (throw (Exception. (str "Cannot expand " input)))))

(defn get-iri-string
  "Given an OWLNamedObject, an IRI, or a string, return its representation
   as a string."
  [input]
  (cond (string? input)
        input
        (instance? OWLNamedObject input)
        (get-iri-string (.getIRI input))
        (instance? IRI input)
        (-> input
            .toString
            (string/replace #"^<|>$" ""))
        :else
        (throw (Exception. (str "Cannot expand " input)))))

(defn shorten
  "Take an object and return a CURIE string (using registered prefixes)
   if possible, otherwise an IRI string."
  [input]
  (let [curie (get-iri-string input)]
    (or (->> @prefixes
             (map (fn [[k v]] [v k]))
             (sort-by (comp count first) >)
             (map #(when (.startsWith curie (first %))
                     (string/replace curie (first %) (second %))))
             (remove nil?)
             first)
        curie)))

(defn in-namespace?
  "Check whether a CURIE string is inside a namespace."
  [namespace curie]
  (if-let [result (get @prefixes (append-colon namespace))]
    (.startsWith (get-iri-string (expand curie)) result)
    false))


;; ## Object Properties

(defn property
  "Take a CURIE string and return an object property. If the argument is
   already an OWLObjectProperty then it is just returned."
  [curie]
  (cond
    (string? curie) (.getOWLObjectProperty data-factory (expand curie))
    (instance? OWLObjectProperty curie) curie
    :else (throw (Exception. (str "Unknown type in owlapi/property for "
                                  curie)))))


;; ## Classes

(defn declare-class!
  "Add a class declaration to the ontology, returning the declaration axiom."
  [ontology curie]
  (let [iri    (expand curie)
        class  (.getOWLClass data-factory iri)
        axiom  (.getOWLDeclarationAxiom data-factory class)]
    (.addAxiom (manager ontology) ontology axiom)
    axiom))

(defn classes
  "List the classes in the ontology as a list of CURIE strings."
  [ontology]
  (map shorten (iterator-seq (.iterator (.getClassesInSignature ontology)))))

(defn class?
  "Check whether the class is in the signature of the ontology."
  [ontology curie]
  (.containsClassInSignature ontology (expand curie)))

(defn class
  "Take a CURIE string and return an OWLClass. If the argument is already an
   OWLClass or OWLClassExpression, it is just returned."
  [curie]
  (cond
    (string? curie) (.getOWLClass data-factory (expand curie))
    (instance? OWLClassExpression curie) curie
    :else (throw (Exception. (str "Unknown type in owlapi/class for " curie)))))


;; ## Class Expressions

(defn union
  "Create an OWLObjectUnionOf class expression from a list of CURIE strings."
  [curies]
  (when (second curies) ; minimum two classes
    (let [classes (map class curies)
          array   (into-array OWLClassExpression classes)]
      (.getOWLObjectUnionOf data-factory array))))

(defn complement
  "Create an OWLObjectComplementOf class expression from a CURIE string."
  [curie]
  (.getOWLObjectComplementOf data-factory (class curie)))

(defn intersection
  "Create an OWLObjectIntersectionOf class expression from a list of CURIE
   strings."
  [curies]
  (when (second curies) ; minimum two classes
    (let [classes (map class curies)
          array   (into-array OWLClassExpression classes)]
      (.getOWLObjectIntersectionOf data-factory array))))

(defn some
  "Create an OWLObjectSomeValuesFrom class expression from the CURIE string
   for a property and either a CURIE string for a class or class expression."
  [property-curie class-curie]
  (.getOWLObjectSomeValuesFrom data-factory
                               (property property-curie)
                               (class class-curie)))

(defn equivalent!
  "Assert that two classes, or a class and a class expression, are
   OWLEquivalentClasses."
  [ontology curie1 curie2]
  (let [axiom (.getOWLEquivalentClassesAxiom
                data-factory (class curie1) (class curie2))]
    (.addAxiom (manager ontology) ontology axiom)
    axiom))

(defn disjoint!
  "Assert that a list of classes are OWLDisjointClasses."
  [ontology curies]
  (when (second curies) ; minimum two classes
    (let [classes (map class curies)
          array   (into-array OWLClassExpression classes)
          axiom   (.getOWLDisjointClassesAxiom data-factory array)]
      (.addAxiom (manager ontology) ontology axiom)
      axiom)))


;; ## Class Hierarchy

(defn children
  "Return a list of CURIE strings of the direct subclasses."
  [ontology curie]
  (->> (.getSubClasses (class curie) ontology)
       .iterator
       iterator-seq
       (filter (partial instance? OWLNamedObject))
       (map shorten)))

(defn descendants
  "Return an unordered list of CURIE string for all direct and indirect
   subclasses."
  [reasoner curie]
  (map shorten
       (-> (class curie)
           (#(.getSubClasses reasoner % false))
           .getFlattened
           .iterator
           iterator-seq)))

(defn parents
  "Return a list of CURIE strings for all the direct superclasses."
  [ontology curie]
  (->> (.getSuperClasses (class curie) ontology)
       (.iterator)
       iterator-seq
       (filter (partial instance? OWLNamedObject))
       (map shorten)))

(defn parent
  "Return the CURIE of the first direct superclass."
  [ontology curie]
  (first (parents ontology curie)))

(defn ns-parent
  "Return one parent (i.e. superclass), preferably from the given namespace."
  [namespace ontology curie]
  (let [parents   (parents ontology curie)
        preferred (filter #(in-namespace? namespace %) parents)]
    (or (first preferred)
        (first parents))))

(defn siblings
  "Return the CURIEs of the sibling classes."
  [ontology curie]
  (let [parent-curie (parent ontology curie)]
    (if parent-curie
      (remove #(= curie %) (children ontology parent-curie)) 
      nil)))

(defn orphan!
  "Remove all superclasses from this class."
  [ontology curie]
  (.removeAxioms (manager ontology)
                 ontology
                 (.getSubClassAxiomsForSubClass ontology (class curie))))

(defn parent!
  "Add this child as a sublclass of the parent."
  [ontology child-curie parent-curie]
  (let [axiom (.getOWLSubClassOfAxiom
                data-factory (class child-curie) (class parent-curie))]
    (.addAxiom (manager ontology) ontology axiom)
    axiom))

(defn ancestry
  "Return a lazy sequence of CURIE strings for all ancestors, starting with
   this CURIE. Uses the `parent` function by default. NOTE: If there are
   multiple parents, only one is chosen!"
  ([ontology curie] (ancestry ontology curie parent))
  ([ontology curie func]
   (let [parent (func ontology curie)]
     (if parent
       (cons curie (lazy-seq (ancestry ontology parent func)))
       [curie]))))

(defn ancestors
  "Return an ordered list of CURIE strings of ancestors, not including this
   class. Uses `ancestry`."
  ([ontology curie] (ancestors ontology curie parent))
  ([ontology curie func] (drop 1 (ancestry ontology curie func))))


;; ## Annotations

(defn literal?
  "Get an OWLLiteral for a value. This is a very basic implementation."
  [value]
  (cond
    (string? value) true
    (instance? Boolean value) true
    (instance? Integer value) true
    (instance? Double value) true
    (instance? Float value) true
    (instance? Long value) true
    :else false))

(defn literal
  "Get an OWLLiteral for a value. This is a very basic implementation."
  [value]
  (.getOWLLiteral data-factory value))

(defn get-value
  "Get a value from a literal, usually a string."
  [value]
  (cond
    (instance? OWLLiteral value) (.getLiteral value)
    (instance? IRI value) (.toString value)
    :else (str value)))

(defn iri-set
  "Given a single CURIE or a sequence of CURIEs, return a set of IRIs."
  [curies]
  (if (sequential? curies)
   (set (map expand curies))
   #{(expand curies)}))

(defn annotation-axioms
  "Get a list of all annotation axioms for a CURIE,
   or restrict to axioms with a given property or properties."
  ([ontology curie]
    (.getAnnotationAssertionAxioms ontology (expand curie)))
  ([ontology curie property-curies]
   (filter #(contains? (iri-set property-curies) (.getIRI (.getProperty %)))
           (annotation-axioms ontology curie))))

;; Get the first annotation axiom, if any.
(def annotation-axiom (comp first annotation-axioms))

(defn property-value
  "Given an OWLAnnotation, return the pair of its property CURIE and its value."
  [annotation]
  [(shorten (.getProperty annotation)) (get-value (.getValue annotation))])

(defn axiom-annotations
  "Given an axiom, get the property, value,
   and a vector of annotation properties and values."
  [axiom]
  [(shorten (.getProperty axiom))
   (get-value (.getValue axiom))
   (mapv property-value (.getAnnotations axiom))])

(defn annotations
  "Get a list of properties and values for all the annotations on this CURIE,
   or restrict the list to just the values of some property or properties."
  ([ontology curie]
   (map property-value (annotation-axioms ontology curie)))
  ([ontology curie property-curies]
   (map #(get-value (.getValue %))
        (annotation-axioms ontology curie property-curies))))

;; Get the first annotation axiom, if any.
(def annotation (comp first annotations))

(defn annotations+
  "Get a list of annotation properties and values for this CURIE
   and all their annotation propeties and values,
   or restrict the list to just the values and annotations for some property,
   or restrict the list to just the annotations for some property and value."
  ([ontology curie]
   (->> (annotation-axioms ontology curie)
        (map axiom-annotations)))
  ([ontology curie property-curie]
   (->> (annotation-axioms ontology curie property-curie)
        (map axiom-annotations)
        (map rest)))
  ([ontology curie property-curie value]
   (->> (annotations+ ontology curie property-curie)
        (filter #(= (first %) value))
        first
        last)))

(defn annotation+
  "Get the first annotated annotation on a CURIE for a given property, if any,
   or restruct to the first annotation for a given property and value."
  ([ontology curie property-curie]
    (first (annotations+ ontology curie property-curie)))
  ([ontology curie property-curie value]
    (first (annotations+ ontology curie property-curie value))))

(defn annotate!
  "Add an annotation to a CURIE with a given property and value."
  [ontology curie property-curie content]
  (let [iri      (expand curie)
        property (.getOWLAnnotationProperty data-factory
                                            (expand property-curie))
        value    (if (literal? content) (literal content) content)
        axiom    (.getOWLAnnotationAssertionAxiom data-factory
                                                  property iri value)]
    (.addAxiom (manager ontology) ontology axiom)
    axiom))

(defn annotate-axiom!
  "Add an annotation to an Axiom with a given property and value."
  [ontology axiom property-curie content]
  (let [property   (.getOWLAnnotationProperty data-factory
                                              (expand property-curie))
        value    (if (literal? content) (literal content) content)
        annotation (.getOWLAnnotation data-factory property value)
        axiom2     (.getAnnotatedAxiom axiom #{annotation})]
    (.removeAxiom (manager ontology) ontology axiom)
    (.addAxiom (manager ontology) ontology axiom2)
    axiom2))

(defn annotate+!
  "Add an annotated annotation to a CURIE with a given property and value,
   and annotation-property and annotation-value."
  [ontology curie property-curie content
   annotation-property-curie annotation-content]
  (let [axiom (annotate! ontology curie property-curie content) ]
    (annotate-axiom! ontology axiom
                     annotation-property-curie annotation-content)))

(defn copy-annotations!
  "Copy all annotation axioms for a CURIE from one ontology to another."
  [from-ontology curie to-ontology]
  (.addAxioms (manager to-ontology)
              to-ontology
              (.getAnnotationAssertionAxioms (class curie) from-ontology)))

(defn remove-annotations!
  "Remove all annotations for a CURIE, or all annotations with some property."
  ([ontology curie]
    (doseq [axiom (annotation-axioms ontology curie)]
      (.removeAxiom (manager ontology) ontology axiom)))
  ([ontology curie property-curie]
    (doseq [axiom (annotation-axioms ontology curie property-curie)]
      (.removeAxiom (manager ontology) ontology axiom))))


;; ## Labels
;; rdfs:label annotations are a common case, so we provide convenience
;; functions.

(defn labels
  "Get a list of the rdfs:labels for a CURIE."
  [ontology curie]
  (annotations ontology curie "rdfs:label"))

(defn label
  "Get the first rdfs:label for a CURIE, if any."
  [ontology curie]
  (annotation ontology curie "rdfs:label"))

(defn label-axioms
  "Get a list of the rdfs:label annotation axioms for a CURIE."
  [ontology curie]
  (filter #(.isLabel (.getProperty %)) (annotation-axioms ontology curie)))

(defn remove-labels!
  "Remove all rdfs:labels from a CURIE."
  [ontology curie]
  (doseq [axiom (label-axioms ontology curie)]
    (.removeAxiom (manager ontology) ontology axiom)))

(defn label!
  "Add an rdfs:label for a CURIE, with an optional annotation."
  ([ontology curie value]
    (annotate! ontology curie "rdfs:label" value))
  ([ontology curie value annotation-property annotation-value]
    (annotate+! ontology curie "rdfs:label" value
                annotation-property annotation-value)))

(defn relabel!
  "Relabel a CURIE. Either removes all other labels,
   or reassigns them to a given property,
   or removes others and annotates the new label,
   or annotations the new label and reassigns to a given property
   with an annotation."
  ([ontology curie value]
   (remove-labels! ontology curie)
   (label! ontology curie value))

  ([ontology curie value reassign-property]
   (let [labels (labels ontology curie)]
     (doseq [label labels]
            (annotate! ontology curie reassign-property label))
     (remove-labels! ontology curie)
     (label! ontology curie value)))

  ([ontology curie value annotation-property annotation-value]
   (let [labels (labels ontology curie)]
     (remove-labels! ontology curie)
     (label! ontology curie value annotation-property annotation-value)))

  ([ontology curie value
    reassign-property reassign-annotation-property reassign-annotation-value]
   (let [labels (labels ontology curie)]
     (doseq [label labels]
            (annotate+! ontology curie reassign-property label
                        reassign-annotation-property reassign-annotation-value))
     (remove-labels! ontology curie)
     (label! ontology curie value)))

  ([ontology curie value
    annotation-property annotation-value
    reassign-property reassign-annotation-property reassign-annotation-value]
   (let [labels (labels ontology curie)]
     (doseq [label labels]
            (annotate+! ontology curie reassign-property label
                        reassign-annotation-property reassign-annotation-value))
     (remove-labels! ontology curie)
     (label! ontology curie value annotation-property annotation-value))))


;; ## Working with Classes

(defn axioms
  "Return a list of axioms for the given entity."
  [ontology curie]
  (iterator-seq (.iterator (.getAxioms ontology (class curie)))))

(defn add-class!
  "Add a class to an ontology, with an optional label and parent."
  ([ontology curie]       (add-class! ontology curie nil nil))
  ([ontology curie label] (add-class! ontology curie nil label))
  ([ontology curie parent-curie label]
    (log/debugf "Adding class '%s'" curie)
    (let [axiom (declare-class! ontology curie)]
      (when parent-curie (parent! ontology curie parent-curie))
      (when label (annotate! ontology curie "rdfs:label" label))
      axiom)))

(defn copy-class!
  "Copy a class, with all axioms and annotations, from one ontology to another."
  [from-ontology target-curie to-ontology]
  (let [target      (class target-curie)
        axioms      (.getAxioms from-ontology target)
        annotations (.getAnnotationAssertionAxioms target from-ontology)]
    (declare-class! to-ontology target-curie)
    (.addAxioms (manager to-ontology) to-ontology axioms)
    (.addAxioms (manager to-ontology) to-ontology annotations)))

(defn remove-class!
  "Remove one class from the given ontology."
  [ontology curie]
  (let [remover (OWLEntityRemover.
                  (manager ontology)
                  (java.util.Collections/singleton ontology))]
    (.accept (class curie) remover)
    (.applyChanges (manager ontology) (.getChanges remover))))


;; ## Reasoners

(defn reasoner
  "Get an instance of the HermiT reasoner for an ontology."
  [ontology]
  (.createReasoner reasoner-factory ontology))

(defn reason!
  "Do reasoning and add the resulting axioms to the ontology"
  [reasoner ontology]
  (let [inferences (into-array InferenceType [InferenceType/CLASS_HIERARCHY])
        generator  (InferredOntologyGenerator. reasoner)]
    (.flush reasoner)
    (.precomputeInferences reasoner inferences)
    (.addGenerator generator (InferredSubClassAxiomGenerator.))
    (.addGenerator generator (InferredEquivalentClassAxiomGenerator.))
    (.fillOntology generator (manager ontology) ontology)
    ontology))

(defn dispose!
  "Dispose of the reasoner."
  [reasoner] (.dispose reasoner))


;; ## Modules

(defn extract
  "Given an ontology, and a list of CURIES,
   return a new ontology with just those CURIEs and their related axioms.
   A bug in older OWLAPI means that we have to remove and then restore all
   subAnnotarionPropertyOf axioms: http://sourceforge.net/p/owlapi/bugs/306/"
  [ontology curies new-iri]
  (let [problems  (.getAxioms ontology AxiomType/SUB_ANNOTATION_PROPERTY_OF)
        pairs     (doall
                    (map (fn [a] [(.getSubProperty a) (.getSuperProperty a)])
                         (iterator-seq (.iterator problems)))) 
        _         (.removeAxioms (manager ontology) ontology problems)
        entities  (set (map class curies)) 
        extractor (SyntacticLocalityModuleExtractor.
                    (manager ontology) ontology ModuleType/STAR)
        axioms    (.extract extractor entities)
        new-ontology (create-ontology new-iri)]
    (.addAxioms (manager ontology) new-ontology axioms)
    (doseq [[sub sup] pairs]
      (.addAxiom (manager ontology)
                 ontology
                 (.getOWLSubAnnotationPropertyOfAxiom data-factory sub sup)))
    new-ontology))


