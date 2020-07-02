(ns edu.boisestate.cs.thqs.core.TypeGraph
  (:require [clojure.spec.alpha :as s]
            [edu.boisestate.cs.thqs.core.types :as types])
  (:import [java.io File]
           [java.util Map]
           [org.jgrapht Graph Graphs]
           [org.jgrapht.graph DefaultDirectedGraph DefaultEdge]
           [soot Scene FastHierarchy]
           [soot.options Options]))

(defrecord TypeGraph [type-set graph])

(defn graph [type-graph]
  "Return the graph portion."
  (:graph type-graph))

(defn type-set [type-graph]
  "Return the map of types."
  (:type-set type-graph))

(defn get-type [^TypeGraph type-graph type]
  "Retrieve the full type map from the `type-graph` via the `type` symbol."
  (get (type-set type-graph) type))

(defn- initialize-soot [classPath mainClass]
  (let [soot-options (Options/v)
        soot-scene (Scene/v)]
    (.set_whole_program soot-options true)
    (.set_allow_phantom_refs soot-options true)
    (.setSootClassPath soot-scene
                       (str (.getSootClassPath soot-scene)
                            (File/pathSeparator)
                            classPath))
    (->> mainClass
         (.loadClassAndSupport soot-scene)
         (.setApplicationClass))
    (.loadNecessaryClasses soot-scene)))

(defn- ->type-set [types]
  (persistent! (reduce
                (fn [acc type] (assoc! acc (:name type) type))
                (transient {})
                types)))

(defn type-set->TypeGraph [type-set]
  (let [graph (DefaultDirectedGraph. DefaultEdge)]
    (doseq [t (vals type-set)]
      (.addVertex graph t))
    (doseq [t (vals type-set)]
      (doseq [i (:interfaces t)]
        (let [interface-type (get type-set i)]
          (if (some? interface-type) (.addEdge graph interface-type t))))
      (let [superclass-sym (:superclass t)
            superclass (get type-set superclass-sym)]
        (if (some? superclass) (.addEdge graph superclass t))))
    (->TypeGraph type-set graph)))

(defn ->primitives-TypeGraph []
  "Construct a TypeGraph containing only Java primitive types."
  (let [primitives-type-set (types/->primitive-types)]
    (type-set->TypeGraph primitives-type-set)))

(defn soot->type-graph [classPath mainClass]
  "Construct type graph from class path and main class via Soot."
  (initialize-soot classPath mainClass)
  (let [soot-scene (Scene/v)
        soot-fast-hierarchy (.getOrMakeFastHierarchy soot-scene)
        soot-classes (into [] (.getClasses soot-scene))
        primitives-type-set (types/->primitive-types)
        types (map types/soot-class->Type soot-classes)
        type-set (->type-set (concat types (vals primitives-type-set)))
        type-graph (DefaultDirectedGraph. DefaultEdge)]
    (doseq [soot-class soot-classes]
      (let [type (types/soot-class->Type soot-class)]
        (.addVertex type-graph type)
        (doseq [soot-sub-type (.getSubclassesOf soot-fast-hierarchy soot-class)]
          (let [sub-type (types/soot-class->Type soot-sub-type)]
            (.addVertex type-graph sub-type)
            (.addEdge type-graph type sub-type)))
        (doseq [soot-sub-interface (.getAllSubinterfaces soot-fast-hierarchy soot-class)]
          (let [sub-interface (types/soot-class->Type soot-sub-interface)]
            (.addVertex type-graph sub-interface)
            (.addEdge type-graph type sub-interface)))
        (doseq [soot-implementer (.getAllImplementersOfInterface soot-fast-hierarchy soot-class)]
          (let [implementer (types/soot-class->Type soot-implementer)]
            (.addVertex type-graph implementer)
            (.addEdge type-graph type implementer)))))
    (Graphs/addGraph type-graph (graph (type-set->TypeGraph primitives-type-set)))
    (->TypeGraph type-set type-graph)))

(defn filter-types [filter? type-graph]
  "Return types selected by the `filter?` predicate.

`filter?` will receive the full type."
  (->> type-graph
       (type-set)
       (filter (fn [[_ type]] (filter? type)))
       (into {})))

(defn remove-types [remove? type-graph]
  "Return the set of types not removed by the `remove?` predicate.

`remove?` will receive the full type."
  (->> type-graph
       (type-set)
       (remove (fn [[_ type]] (remove? type)))
       (into {})))

(defn application-types [type-graph]
  "Return the set of application types.

This will filter out all of the typical Java types based on the prefix of the
types in the provided `type-graph`."
  (remove-types
   (fn [type] (or (types/core-java-type? (:name type))
                 (types/primitive-type? type)))
   type-graph))
