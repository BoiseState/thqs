(ns edu.boisestate.cs.thqs.core.visualization
  (:use [clojure.java.io])
  (:require [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph])
  (:import [java.io StringWriter]
           [org.jgrapht Graph]
           [org.jgrapht.nio.dot DOTExporter]
           [org.jgrapht.nio DefaultAttribute]))

(defn- ^java.util.function.Function as-java-function [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))

(defn- typeShape [t]
  (cond (= (:form t) :concrete) "rectangle"
        (= (:form t) :interface) "parallelogram"
        (= (:form t) :abstract) "diamond"
        (= (:form t) :primitive) "trapezium"
        :else "egg"))

(defn- type->dot-id [t]
  (-> (str (:name t))
      (clojure.string/replace "." "_")
      (clojure.string/replace "$" "_")))

(defn generateDotGraph [^Graph graph]
  "Export graph to DOT string."
  (let [exporter (DOTExporter. (as-java-function type->dot-id))
        writer (StringWriter.)]
    (.setVertexAttributeProvider
     exporter
     (as-java-function (fn [t] {"label" (DefaultAttribute/createAttribute (str (:name t)))
                               "shape" (DefaultAttribute/createAttribute (typeShape t))})))
    (.exportGraph exporter graph writer)
    (.toString writer)))

(defn generateDotGraphToFile [^Graph graph fileName]
  "Export graph to DOT and write to file."
  (with-open [fileHandle (writer fileName :append false)]
    (.write fileHandle (generateDotGraph graph))))
