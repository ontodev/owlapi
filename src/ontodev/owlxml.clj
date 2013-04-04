;; # XML Stream Processing
;;
;; OWL ontologies are usually saved in RDF/XML format. These files can be
;; very large, and slow to load and save using OWLAPI.
;; This file contains tools for operating on OWL files as XML streams,
;; using a streaming XML processing library. The result is more fragile,
;; but uses much less memory and works very quickly.
;;
;; For an overview of working with the Woodstox XML stream processor, see
;; http://www.developerfusion.com/article/84523/stax-the-odds-with-woodstox/

;; We import classes from `javax.xml` and `org.codehaus.stax2` (Woodstox).
(ns ontodev.owlxml
  (:require clojure.set)
  (:import (javax.xml.namespace QName)
           (javax.xml.stream StreamFilter XMLStreamException)
           (org.codehaus.stax2 XMLInputFactory2 XMLOutputFactory2
                               XMLStreamReader2 XMLStreamWriter2)))

;; ## Clojure Type Hints
;;
;; Java is a strongly typed programming language, in which the type of every
;; variable and method is explicitly declared. Clojure is a dynamic language
;; in which types are usually inferred. The type inference process often 
;; involves "reflection" into classes, and this tends to slow down Clojure code.
;;
;; Clojure can be almost as fast as native Java code, but we have to avoid 
;; reflection. We avoid it by adding "type hints" to tell the Clojure compiler
;; what types to expect for input and return values. Metadata annotation on
;; functions and values provide the hints: `^QName`, `^XMLInputFactory2`, etc.

;; The code in this file is optimized for speed, so we tell the Clojure compiler
;; to `warn-on-reflection` and add type hints until those warnings go away.
(set! *warn-on-reflection* true)

;; ## Namespaces and QNames
;; We define namespaces and names that we'll be using.
(def rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(def rdfs "http://www.w3.org/2000/01/rdf-schema#")
(def owl "http://www.w3.org/2002/07/owl#")

(def ^QName owl:Ontology (QName. owl "Ontology"))
(def ^QName owl:Class (QName. owl "Class"))
(def ^QName owl:Axiom (QName. owl "Axiom"))
(def ^QName owl:annotatedSource (QName. owl "annotatedSource"))
(def ^QName owl:annotatedProperty (QName. owl "annotatedProperty"))
(def ^QName owl:annotatedTarget (QName. owl "annotatedTarget"))
(def ^QName owl:someValuesFrom (QName. owl "someValuesFrom"))
(def ^QName rdf:Description (QName. rdf "Description"))
(def ^QName rdfs:subClassOf (QName. rdfs "subClassOf"))


;; ## Factories
;;
;; The following functions are used to create factories, readers, writers, 
;; and filters from the Woodstox library.

(defn create-input-factory
  "Create and return Woodstox XMLInputFactory2, with configuration."
  ^XMLInputFactory2 []
  (XMLInputFactory2/newInstance))

;; By default we will use the same shared input factory.
(def ^XMLInputFactory2 shared-input-factory (create-input-factory))

(defn create-output-factory
  "Create and return Woodstox XMLOutputFactory2."
  ^XMLOutputFactory2 []
  (XMLOutputFactory2/newInstance))

;; By default we will use the same shared output factory.
(def ^XMLOutputFactory2 shared-output-factory (create-output-factory))

(defn create-stream-reader
  "Create and return a Woodstox XMLStreamReader2 for a given path.
   An XMLInputFactory is optional."
  (^XMLStreamReader2 [path]
    (create-stream-reader shared-input-factory path))
  (^XMLStreamReader2 [^XMLInputFactory2 input-factory path] 
    (.createXMLStreamReader input-factory (clojure.java.io/file path))))

(defn create-stream-writer
  "Create and return a Woodstox XMLStreamWriter2 for a given path.
   An XMLOutputFactory is optional."
  (^XMLStreamWriter2 [path]
    (create-stream-writer shared-output-factory path))
  (^XMLStreamWriter2 [^XMLOutputFactory2 output-factory path] 
    (.createXMLStreamWriter output-factory (clojure.java.io/writer path))))

(defn create-stream-filter
  "Take a boolean function (with a reader as its argument) and return an
   implementation of the StreamFilter interface."
  ^StreamFilter [boolean-function]
   (proxy [StreamFilter] []
          (accept [^XMLStreamReader2 reader]
                  (boolean-function reader))))

(defn create-filtered-stream-reader
  "Create and return a Woodstox XMLStreamReader2 with a given StreamFilter.
   An XMLInputFactory is optional."
  (^XMLStreamReader2 [path ^StreamFilter stream-filter]
    (create-filtered-stream-reader shared-input-factory path stream-filter))
  (^XMLStreamReader2 [^XMLInputFactory2 input-factory
                      path
                      ^StreamFilter stream-filter] 
    (.createFilteredReader input-factory
                           (create-stream-reader path)
                           stream-filter)))

;; ## Ontology Processing Utilities
;;
;; These functions use Woodstox tools to process OWL ontologies.

(defn filter-classes
  "A boolean-function that takes a reader and returns true only if the event
   is a startElement and its name is owl:Class or rdfs:subClassOf."
  [^XMLStreamReader2 reader]
  (and (.isStartElement reader)
       (let [name (.getName reader)]
         (or (.equals name owl:Class)
             (.equals name rdfs:subClassOf)))))

(defn filter-resources
  "Slightly more general than `filter-classes`:
   also includes `owl:someValuesFrom`"
  [^XMLStreamReader2 reader]
  (and (.isStartElement reader)
       (let [name (.getName reader)]
         (or (.equals name owl:Class)
             (.equals name rdf:Description)    
             (.equals name rdfs:subClassOf)    
             (.equals name owl:someValuesFrom)))))

(defn copy-element
  "Use Woodstox's `copyEventFromReader` (very fast) to copy an element from
   the reader to the writer with whitespace and comments."
  [^XMLStreamReader2 reader ^XMLStreamWriter2 writer about]
  (let [depth (.getDepth reader)]
    ; Write a comment with the IRI for this block.
    (when about
      (doto writer
        (.writeCharacters "\n\n\n    ") 
        (.writeComment (str " " about " ")) 
        (.writeCharacters "\n")))
    (.writeCharacters writer "\n    ")
    (while (>= (.getDepth reader) depth)
      (.copyEventFromReader writer reader true) 
      (.next reader))))

;; ### Handle owl:Axioms
;; OWL annotation axioms have their own top-level elements. Unfortunately,
;; the information about the 'source' of the annotation is within a child of
;; the element. Since streaming XML processors can't go backwards, we have to
;; store a bunch of information about the annotation axiom, then decide 
;; whether or not to copy the axiom.

;; The alternative to this implementation would be is an EventStream,
;; allowing us to save objects.

(defn write-axiom
  "Take a map of information and write an owl:Axiom element."
  [^XMLStreamWriter2 w coll]
  (try
    (.writeCharacters w "\n    ")
    (.writeStartElement w "owl" "Axiom" owl)
    
    ; annotatedTarget
    (.writeCharacters w "\n        ")
    (.writeStartElement w "owl" "annotatedTarget" owl)
    (.writeAttribute w "rdf" rdf "datatype" (:target-type coll))
    (.writeCharacters w (:target-value coll))
    (.writeEndElement w)

    ; annotatedSource
    (.writeCharacters w "\n        ")
    (.writeStartElement w "owl" "annotatedSource" owl)
    (.writeAttribute w "rdf" rdf "resource" (:source coll))
    (.writeEndElement w)

    ; annotations
    (doseq [ann (:annotations coll)]
      (.writeCharacters w "\n        ") 
      (.writeStartElement w (:prefix ann) (:local-name ann) (:namespace ann)) 
      (when (:datatype ann)
        (.writeAttribute w "rdf" rdf "datatype" (:datatype ann))) 
      (when (:resource ann)
        (.writeAttribute w "rdf" rdf "resource" (:resource ann))) 
      (when (:content ann)
        (.writeCharacters w (:content ann))) 
      (.writeEndElement w)) 
    
    ; annotatedProperty
    (.writeCharacters w "\n        ")
    (.writeStartElement w "owl" "annotatedProperty" owl)
    (.writeAttribute w "rdf" rdf "resource" (:property coll))
    (.writeEndElement w)

    ; end element
    (.writeCharacters w "\n    ")
    (.writeEndElement w)
    (catch Exception e (println "Exception while wrting to write axiom:" coll))))

(defn check-axiom
  "Take a reader, writer, and a set of IRIs to copy.
   Read the content of the current owl:Axiom element.
   If the axiom is for a class in the set, write it using write-axiom.
   Otherwise just return without writing anything."
  [^XMLStreamReader2 reader ^XMLStreamWriter2 writer classes]
  (let [depth (.getDepth reader)
        coll  (atom {:annotations []})]
    (while (>= (.getDepth reader) depth)
      (when (.isStartElement reader)
        (let [name (.getName reader)]
          (cond
            (.equals name owl:annotatedSource)
              (swap! coll assoc
                     :source (.getAttributeValue reader rdf "resource"))
            (.equals name owl:annotatedProperty)
              (swap! coll assoc
                     :property (.getAttributeValue reader rdf "resource"))
            (.equals name owl:annotatedTarget)
              (swap! coll assoc
                     :target-type (.getAttributeValue reader rdf "datatype")
                     :target-value (.getElementText reader))
            (.getAttributeValue reader rdf "datatype")
              (swap! coll update-in [:annotations] conj {
                     :prefix     (.getPrefix reader)
                     :namespace  (.getNamespaceURI reader)
                     :local-name (.getLocalName reader)
                     :datatype   (.getAttributeValue reader rdf "datatype")
                     :content    (do (.next reader)
                                   (when (.hasText reader)
                                     (.getText reader)))})   
            :else
              (swap! coll update-in [:annotations] conj {
                     :prefix     (.getPrefix reader)
                     :namespace  (.getNamespaceURI reader)
                     :local-name (.getLocalName reader)
                     :resource   (.getAttributeValue reader rdf "resource")}))))
      (.next reader))
    ; Now decide whether to include the owl:Axiom or not.
    (when (contains? classes (:source @coll))
      (write-axiom writer @coll))))

(defn check-element
  "Take a reader, writer, and a set of IRIs to copy, and an `else-copy` flag.
   If the current element is an owl:Class in the set, copy it.
   Otherwise, copy the element only if `else-copy` is true."
  [^XMLStreamReader2 reader ^XMLStreamWriter2 writer classes else-copy]
  (let [name  (.getName reader)
        about (.getAttributeValue reader rdf "about")]
    (cond
      (.equals name owl:Class)
        (if (contains? classes about)
          (copy-element reader writer about)
          (.skipElement reader))
      (.equals name owl:Axiom) (check-axiom reader writer classes)
      else-copy (copy-element reader writer about))))

;; ## Ontology Processing Functions
;; These are the high-level functions for extracting structures from OWL files.

(defn extract-hierarchy
  "Given a path, read the file and return a map of all subClassOf pairs from 
   child to parent. Note that this only works for a mono-hierarchy!"
  [path]
  (let [sf      (create-stream-filter filter-classes)
        reader  (create-filtered-stream-reader path sf)
        class   (atom nil)
        results (atom {})]
    (while (.hasNext reader)
      (if (.equals owl:Class (.getName reader))
        (let [iri (.getAttributeValue reader rdf "about")]
          (swap! results assoc iri nil) 
          (reset! class iri))
        (do
          (swap! results assoc @class
                 (.getAttributeValue reader rdf "resource"))
          (reset! class nil))) 
      (.next reader))
    (.close reader)  
    @results))

(defn extract-resources
  "Slightly more general than `extract-hierarchy`. Returns a map from class
   IRIs to sets of IRIs from `rdfs:subClassOf` and `owl:someValuesFrom` nodes."
  [path]
  (let [sf      (create-stream-filter filter-resources)
        reader  (create-filtered-stream-reader path sf)
        class   (atom nil)
        results (atom {})]
    (while (.hasNext reader)
      (if (.equals owl:Class (.getName reader))
        (let [iri (.getAttributeValue reader rdf "about")]
          (when iri 
            (when-not (find @results iri) 
              (swap! results assoc iri #{})) 
            (reset! class iri)))
        (let [resource (.getAttributeValue reader rdf "resource")
              about    (.getAttributeValue reader rdf "about")]
          (when (and @class resource) 
            (swap! results update-in [@class] conj resource))
          (when (and @class about) 
            (swap! results update-in [@class] conj about))))  
      (.next reader))
    (.close reader)
    @results))


;; ## Utility Functions
;; These are tools for working with hierarchy and resource collections, 
;; extracted above.

(defn ancestry
  "Given a map of child-parent pairs and a descendant, return a lazy list of
   ancestors starting with the descendant."
  [coll item]
  (if (get coll item)
    (cons item (lazy-seq (ancestry coll (get coll item))))
    [item]))

(defn extract-ancestry-one
  "Given a collection of child-parent pairs and an item, return a set of all
   ancestors. Check for loops."
  [coll item]
  (let [n         100
        ancestors (take n (ancestry coll item))]
    (when (= n (count ancestors))
      (println "ERROR: There seems to be a loop in the ancestry of"
               item (set ancestors))
      (throw (Exception. "Loop detected in extract-ancestry-one for" item)))
    (set ancestors)))

(defn extract-ancestor-closure
  "Given a map of child-parent pairs and a list of descendants, return a set of
   all descendants and all their ancestors."
  [coll descendants]
  (apply clojure.set/union
         (map (partial extract-ancestry-one coll) descendants)))

(defn extract-resource-one
  [resources coll item]
  (if item
    (let [items-found  (get resources item)
          items-added  (clojure.set/union coll #{item})
          items-to-get (clojure.set/difference items-found items-added)]
      #_(println "RESOURCES" coll item items-found items-to-get)
      (apply clojure.set/union
             items-added
             (map (partial extract-resource-one resources items-added)
                  items-to-get)))
    coll))

(defn extract-resource-closure
  "Given a map of item-items pairs and a list of target items, return a set of
   all items required to complete the closure."
  [resources items]
  (reduce (partial extract-resource-one resources) #{} items))

(defn longest
  "Given two sequences, return the longest one."
  [xs ys]
  (if (> (count xs) (count ys)) xs ys))
 
; Find the longest common initial subsequence among a list of sequences.
(def longest-initial-subsequence
  (memoize
   (fn [& seqs]
    (cond
      ; some sequence lacks a first element 
      (some nil? (map first seqs)) nil
      ; all first elements are equal 
      (apply = (map first seqs))
        (cons (first (first seqs))
              (apply longest-initial-subsequence (map rest seqs)))
      :else nil))))

(defn common-ancestry
  "Find the longest sequence of common ancestors for a set of curies."
  [ontology parent-fn curies] 
  (->> (map #(ancestry ontology % parent-fn) curies)
       (map reverse)
       (apply longest-initial-subsequence)
       reverse))


;; ## Extract Ontology Classes
;; Use the functions to extract or copy sets of classes from an ontology.

(defn extract-classes
  "Take an input path, an output path, new IRI, and a set of IRIs for classes.
   Read the input file, copy everything to the output file, except for
   owl:Class and owl:Axiom -- only the classes and axioms with IRIs in the
   set are copied to the output file.
   Set the owl:Ontology path for the new file to the new IRI."
  [from-path to-path to-iri classes-coll]
  (let [^XMLStreamReader2 reader (create-stream-reader from-path)
        ^XMLStreamWriter2 writer (create-stream-writer to-path)
        classes (set classes-coll)]

    ; Copy everything before the root element.
    (while (not= 1 (.getEventType reader))
      (.copyEventFromReader writer reader true)
      (.next reader))
    (.writeCharacters writer "\n")
    
    ; Copy the root element's startElement event.
    (.copyEventFromReader writer reader true)

    ; Check all children of the root element.
    ; 1. copy comment blocks for sections, e.g "Classes"
    ; 2. check elements
    (while (.hasNext reader)
      (.next reader)
      (when (and (= 5 (.getEventType reader)) ; isComment
                 (.startsWith (.trim (.getText reader)) "////")) 
        (.writeCharacters writer "\n\n\n    ") 
        (.copyEventFromReader writer reader true))
      (when (.isStartElement reader)
        (if (.equals owl:Ontology (.getName reader))
          (do ; set IRI for new ontology
            (.skipElement reader) 
            (doto writer
              (.writeCharacters "\n    ")
              (.writeStartElement "owl" "Ontology" owl)
              (.writeAttribute "rdf" rdf "about" to-iri)
              (.writeEndElement)))
          (check-element reader writer classes true))))

    (.writeCharacters writer "\n\n")
    (.close writer)
    (.close reader)))

(defn extract-classes-into
  "As `extract-classes`, but start by copying the full contents of `source`,
   and ignore all elements in `target` that aren't in `classes`."
  [source-path target-path to-path to-iri classes-coll]
  (let [^XMLStreamReader2 source (create-stream-reader source-path)
        ^XMLStreamReader2 target (create-stream-reader target-path)
        ^XMLStreamWriter2 writer (create-stream-writer to-path)
        classes (set classes-coll)] 
    
    ; source
    ; Copy everything from except the root's endElement.
    (while (not (and (= 2 (.getEventType source)) ; endElement
                     (= 1 (.getDepth source)))) 
      (.copyEventFromReader writer source true)
      (.next source))
    (.close source)  
    
    ; target
    ; Skip everything before the root element.
    (while (not= 1 (.getEventType target))
      (.next target))

    ; Check all children of the root element.
    (while (.hasNext target)
      (.next target)
      (when (.isStartElement target)
        (if (.equals owl:Ontology (.getName target))
          (do ; set IRI for new ontology
            (.skipElement target) 
            (doto writer
              (.writeCharacters "\n    ")
              (.writeStartElement "owl" "Ontology" owl)
              (.writeAttribute "rdf" rdf "about" to-iri)
              (.writeEndElement)))
          (check-element target writer classes false))))

    (.writeCharacters writer "\n\n")
    (.close writer)
    (.close target)))


