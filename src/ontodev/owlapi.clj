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
            [clojure.java.io :as io])
  (:import
    (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                  OWLClassExpression OWLClass OWLAnnotation
                                  OWLAxiom OWLAnnotationAssertionAxiom
                                  OWLNamedObject OWLLiteral OWLObjectProperty)
    (org.semanticweb.owlapi.apibinding OWLManager)
    (org.semanticweb.owlapi.io RDFXMLOntologyFormat)
    (org.semanticweb.owlapi.util DefaultPrefixManager)
    (org.semanticweb.owlapi.reasoner InferenceType)
    (org.semanticweb.owlapi.util InferredAxiomGenerator
                                 InferredSubClassAxiomGenerator
                                 InferredEquivalentClassAxiomGenerator
                                 InferredDisjointClassesAxiomGenerator
                                 InferredClassAssertionAxiomGenerator
                                 InferredOntologyGenerator
                                 OWLOntologyMerger)
    (org.semanticweb.HermiT Reasoner Reasoner$ReasonerFactory)))


;; ## Globals
;; In the current implementation all ontologies share a common OWLDataFactory
;; and OWLOntologyManager.
(def reasoner-factory (Reasoner$ReasonerFactory.))
(def data-factory     (OWLManager/getOWLDataFactory))
(def manager          (OWLManager/createOWLOntologyManager data-factory))
(def prefixes         (DefaultPrefixManager.))


;; ## Ontologies
;; Create, load, save, and remove ontologies.
(defn create-ontology
  [iri]
  (log/info "Creating ontology:" iri)
  (.createOntology manager (IRI/create iri)))

(defn load-ontology
  [path]
  (log/info "Loading ontology:" path)
  (.loadOntologyFromOntologyDocument manager (io/file path)))

(defn save-ontology
  [ontology path]
  (log/info "Saving ontology:" path)
  (.saveOntology manager ontology (IRI/create (io/as-url (io/file path)))))

(defn remove-ontology
  [ontology]
  (log/info "Removing ontology:" ontology)
  (.removeOntology manager ontology))

(defn merge-ontologies
  "Load one or more ontologies from files, merge with a new IRI, and save to
   a new file. Uses a new manager and data-factory, and removes all loaded
   ontologies."
  [iri save-path & merge-paths]
  (let [data-factory (OWLManager/getOWLDataFactory)
        manager      (OWLManager/createOWLOntologyManager data-factory)
        load-fn      (fn [path]
                         (log/info "Loading ontology for merge:" path)
                         (.loadOntologyFromOntologyDocument
                           manager (io/file path)))
        loaded       (doall (map load-fn merge-paths))]
    (log/info "Creating ontology:" iri)
    (let [merger (OWLOntologyMerger. manager)
          merged (.createMergedOntology merger manager (IRI/create iri))]
      (log/info "Saving ontology:" save-path)
      (.saveOntology manager merged
                     (IRI/create (io/as-url (io/file save-path))))
      (doseq [ontology loaded] (.removeOntology manager ontology))
      (.removeOntology manager merged)
      nil)))


;; ## IRIs
;; It's more convenient to work with CURIE strings, but that means we have to
;; convert back and forth between strings, IRIs, and objects.

(defn append-colon
  "Append a colon to the string if required."
  [s] (if (.endsWith s ":") s (str s ":")))

(defn add-prefix
  "Add a prefix to the shared prefix manager."
  [prefix iri]
  (.setPrefix prefixes (append-colon prefix) iri))

(defn expand
  "Take a CURIE string or OWLNamedObject and return an IRI."
  [curie]
  (cond (and (string? curie) (.startsWith curie "http:")) (IRI/create curie)
        (string? curie) (.getIRI prefixes curie)
        (number? curie) (.getIRI prefixes (str curie))
        (instance? OWLNamedObject curie) (.getIRI curie)
        :else (throw (Exception. (str "Cannot expand " curie)))))

(defn shorten
  "Take an object and return a CURIE string."
  [curie]
  (cond (string? curie)
          (-> (.getShortForm prefixes (IRI/create curie))
              .toString
              (string/replace #"^<|>$" "")) ; have to strip angle brackets
        (instance? OWLNamedObject curie)
          (string/replace (.toString (.getShortForm prefixes curie))
                          #"^<|>$" "")
        :else (throw (Exception. (str "Cannot shorten " curie)))))

(defn in-namespace?
  "Check whether a CURIE string is inside a namespace."
  [namespace curie]
  (.startsWith (.toString (expand curie))
               (if (.containsPrefixMapping prefixes (append-colon namespace))
                 (.getPrefix prefixes (append-colon namespace))
                 namespace)))

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
    (.addAxiom manager ontology axiom)
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
    (.addAxiom manager ontology axiom)
    axiom))

(defn disjoint!
  "Assert that a list of classes are OWLDisjointClasses."
  [ontology curies]
  (when (second curies) ; minimum two classes
    (let [classes (map class curies)
          array   (into-array OWLClassExpression classes)
          axiom   (.getOWLDisjointClassesAxiom data-factory array)]
      (.addAxiom manager ontology axiom)
      axiom)))


;; # Class Hierarchy

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

(defn orphan!
  "Remove all superclasses from this class."
  [ontology curie]
  (.removeAxioms manager ontology
                 (.getSubClassAxiomsForSubClass ontology (class curie))))

(defn parent!
  "Add this child as a sublclass of the parent."
  [ontology child-curie parent-curie]
  (let [axiom (.getOWLSubClassOfAxiom
                data-factory (class child-curie) (class parent-curie))]
    (.addAxiom manager ontology axiom)
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

(defn annotation-axioms
  "Get a list of all annotation axioms for a CURIE,
   or restrict to axioms with a given property."
  ([ontology curie]
    (iterator-seq
      (.iterator (.getAnnotationAssertionAxioms ontology (expand curie)))))
  ([ontology curie property-curie]
   (let [property-iri (expand property-curie)
         axioms       (annotation-axioms ontology curie)]
     (filter #(= property-iri (.getIRI (.getProperty %))) axioms))))

;; Get the first annotation axiom, if any.
(def annotation-axiom (comp first annotation-axioms))

(defn annotations
  "Get a list of properties and values for all the annotations on this CURIE,
   or restrict the list to just the values of some property."
  ([ontology curie]
    (let [class (class curie)
          annotations (iterator-seq
                        (.iterator (.getAnnotations class ontology)))]
      (map #(vector (shorten (.getProperty %)) (get-value (.getValue %)))
           annotations)))
  ([ontology curie property-curie]
    (let [class    (class curie)
          property (.getOWLAnnotationProperty data-factory
                                              (expand property-curie))
          annotations (iterator-seq
                        (.iterator (.getAnnotations class ontology property)))]
      (map #(get-value (.getValue %)) annotations))))

;; Get the first annotation axiom, if any.
(def annotation (comp first annotations))

(defn axiom-annotations
  "Given an axiom, get the property, value,
   and a vector of annotation properties and values."
  [axiom]
  [(shorten (.getProperty axiom))
   (get-value (.getValue axiom))
   (vec (map #(vector (shorten (.getProperty %)) (get-value (.getValue %)))
             (iterator-seq (.iterator (.getAnnotations axiom)))))])

(defn annotations+
  "Get a list of annotation properties and values for this CURIE
   and all their annotation propeties and values,
   or restrict the list to just the values and annotations for some property,
   or restrict the list to just the annotations for some property and value."
  ([ontology curie]
    (let [axioms (annotation-axioms ontology curie)]
      (map axiom-annotations axioms)))
  ([ontology curie property-curie]
    (let [axioms (annotation-axioms ontology curie property-curie)]
      (map rest (map axiom-annotations axioms))))
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
        value    (if (string? content)
                   (.getOWLLiteral data-factory content)
                   content)
        axiom    (.getOWLAnnotationAssertionAxiom data-factory
                                                  property iri value)]
    (.addAxiom manager ontology axiom)
    axiom))

(defn annotate-axiom!
  "Add an annotation to an Axiom with a given property and value."
  [ontology axiom property-curie content]
  (let [property   (.getOWLAnnotationProperty data-factory
                                              (expand property-curie))
        value      (if (string? content)
                     (.getOWLLiteral data-factory content)
                     content)
        annotation (.getOWLAnnotation data-factory property value)
        axiom2     (.getAnnotatedAxiom axiom #{annotation})]
    (.removeAxiom manager ontology axiom)
    (.addAxiom manager ontology axiom2)
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
  (.addAxioms manager to-ontology
              (.getAnnotationAssertionAxioms (class curie) from-ontology)))

(defn remove-annotations!
  "Remove all annotations for a CURIE, or all annotations with some property."
  ([ontology curie]
    (doseq [axiom (annotation-axioms ontology curie)]
      (.removeAxiom manager ontology axiom)))
  ([ontology curie property-curie]
    (doseq [axiom (annotation-axioms ontology curie property-curie)]
      (.removeAxiom manager ontology axiom))))


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
    (.removeAxiom manager ontology axiom)))

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
    (.addAxioms manager to-ontology axioms)
    (.addAxioms manager to-ontology annotations)))


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
    (.fillOntology generator manager ontology)
    ontology))

(defn dispose!
  "Dispose of the reasoner."
  [reasoner] (.dispose reasoner))



