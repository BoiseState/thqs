(ns edu.boisestate.cs.thqs.main.Main
  (:require [edu.boisestate.cs.thqs.core.types :as types]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]
            [edu.boisestate.cs.thqs.core.query :as query]
            [edu.boisestate.cs.thqs.core.similarity :as sim]
            [clojure.edn :as edn])
  (:gen-class))

(def usage (str "thqs: type hierarchy query system"
                "GPL-3.0 or later WITH classpath exception"))

(defn- zip-application-classes [app-classes]
  (for [A app-classes
        B (drop-while (fn [b] (not (= b A))) app-classes)]
    [A B]))

(defn- matcher-fn [match? N type-graph]
  (fn [[A B]] (into [(types/type-name A) (types/type-name B)]
                   (match? A B N type-graph))))

(defn- unrelated-classes? [a b type-graph]
  (let [A (types/type-name a)
        B (types/type-name b)]
    (= (query/distance-from-structural-common-ancestors A B type-graph) ##Inf)))

(defn- get-package [main-class]
  (.substring main-class 0 (.lastIndexOf main-class ".")))

(defn- package-class-fn [package]
  (fn [type]
    (let [type-name (str (types/type-name type))]
      (.startsWith type-name package))))

(defn structural-comparison [N type-graph]
  (map (matcher-fn sim/structural-similarity-match? N type-graph)))

(defn structural-comparison-wo-projection [N type-graph]
  (map (matcher-fn sim/structural-similarity-match-wo-projection? N type-graph)))

(defn- pp-results [results]
  (doseq [result results]
    (println (apply (partial format "%s ~ %s\t%b\t%d\t%d") result))))

(defn- do-comparison [classpath main-class N flatten-hierarchy?]
  (let [type-graph (TypeGraph/soot->type-graph classpath main-class)
        zipped-pairs (->> (TypeGraph/application-types type-graph)
                          (vals)
                          (zip-application-classes)
                          (filter (fn [[a b]] (unrelated-classes? a b type-graph))))]
    (if flatten-hierarchy?
      (pp-results (sequence (structural-comparison N type-graph)
                            zipped-pairs))
      (pp-results (sequence (structural-comparison-wo-projection N type-graph)
                            zipped-pairs)))))

(defn -main
  "Main Entry Point, a dispatcher of sorts"
  ([]
   (binding [*out* *err*]
     (println "No arguments passed")))
  ([^java.lang.String classpath
    ^java.lang.String main-class
    ^java.lang.String level]
   (let [N (edn/read-string level)]
     (do-comparison classpath main-class N true)))
  ([^java.lang.String classpath
    ^java.lang.String main-class
    ^java.lang.String level
    ^java.lang.String flatten-arg]
   (let [N (edn/read-string level)
         flatten-hierarchy? (edn/read-string flatten-arg)]
     (do-comparison classpath main-class N flatten-hierarchy?)))
  ([_]
   (binding [*out* *err*]
     (println "Too many arguments"))))
