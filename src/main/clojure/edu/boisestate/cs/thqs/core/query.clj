(ns edu.boisestate.cs.thqs.core.query
  (:require [clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]
            [edu.boisestate.cs.thqs.core.types :as types])
  (:import [org.jgrapht Graphs]
           [org.jgrapht.graph EdgeReversedGraph]
           [org.jgrapht.alg.lca NaiveLCAFinder]
           [org.jgrapht.alg.shortestpath BFSShortestPath]))

(defn get-common-ancestors [a b type-graph]
  "Find the common ancestor between nodes `a` and `b` in `type-graph`.

`type-graph` should be the full graph of types and the edges should point from
parents to children."
  (let [finder (NaiveLCAFinder. (TypeGraph/graph type-graph))]
    (->> (.getLCASet finder a b)
         (into #{}))))

(defn get-structural-common-ancestors [a b type-graph]
  "Find the set of common ancestors between `a` and `b` that do not include
  interfaces."
  (let [ancestors (get-common-ancestors a b type-graph)]
    (->> ancestors
         (filter (fn [t] (not (= (:form t) :interface))))
         (into #{}))))

(defn get-ancestor-set [a type-graph]
  (let [graph (TypeGraph/graph type-graph)
        get-predecessors (fn [t] (into #{} (Graphs/predecessorListOf graph t)))]
    (set/union #{a}
               (loop [predecessors (get-predecessors a)
                      accumulator predecessors]
                 (cond (empty? predecessors) accumulator
                       :else
                       (let [b (first predecessors)
                             b-predecessors (get-predecessors b)]
                         (recur (set/union b-predecessors (rest predecessors))
                                (set/union accumulator b-predecessors))))))))

(defn get-structural-ancestors [a type-graph]
  "Return the set of ancestors that are not interfaces."
  (->> (get-ancestor-set a type-graph)
       (filter (fn [t] (not (= (:form t) :interface))))
       (into #{})))

(defn get-interfaces [a type-graph]
  "Return set of interfaces for class `a` in `graph`."
  (let [graph (TypeGraph/graph type-graph)
        ancestors (get-ancestor-set a graph)]
    (into #{} (filter (fn [t] (= (:form t) :interface)) ancestors))))

(defn geodesic-distance [a b type-graph]
  (let [graph (TypeGraph/graph type-graph)
        undirected-graph (Graphs/undirectedGraph graph)
        shortest-path (BFSShortestPath. undirected-graph)]
    (some->>
     (.getPath shortest-path a b)
     (.getLength))))

(defn distance-from-common-ancestor [a b c type-graph]
  "Calculate the distance between `a` and `b` the common ancestor.

In cases where there is no common ancestor or the common ancestor is Object, we
return infinite.  Otherwise, return the shortest (undirected) path between the
two types."
   (cond (nil? c) ##Inf
         (= 'java.lang.Object (:name c)) ##Inf
         :else (let [a-c-distance (geodesic-distance a c type-graph)
                     b-c-distance (geodesic-distance b c type-graph)]
                 (+ a-c-distance b-c-distance))))

(defn distances-from-common-ancestors [a b type-graph]
  "Create a transducer over common ancestors of `a` and `b`."
  (map (fn [c] (list (:name c) (distance-from-common-ancestor a b c type-graph)))))

(defn distance-from-structural-common-ancestors [a b type-graph]
  (let [cs (get-structural-common-ancestors a b type-graph)
        ad-pairs (->> cs
                      (transduce (distances-from-common-ancestors a b type-graph) conj)
                      (filter some?))]
    (cond (empty? ad-pairs) nil
          :else (apply min-key second ad-pairs))))

(defn- resolve-fields-from-ancestor [class ancestor type-graph]
  (let [graph (TypeGraph/graph type-graph)
        shortest-path (BFSShortestPath. graph)]
    (some->>
     (.getPath shortest-path ancestor class)
     (.getEdgeList)
     (map (fn [edge] (.getEdgeSource graph edge)))
     (mapcat (fn [class] (:fields class))))))

(defn resolve-fields [class ancestors type-graph]
  "Resolve fields between the given class `c` and `ancestor` using the provided type hierarchy graph.

Return a complete list of fields for the provided soot class."
  (some->>
   ancestors
   (map (fn [ancestor] (resolve-fields-from-ancestor class ancestor type-graph)))
   (conj (:fields class))
   (flatten)
   (filter some?)))

(defn resolve-fields-for-pair [a b type-graph]
  "Resolve the pair of fields between two classes."
  (let [ancestors (get-common-ancestors a b type-graph)
        a-fields (resolve-fields a ancestors type-graph)
        b-fields (resolve-fields b ancestors type-graph)]
    (list a-fields b-fields)))

(defn get-field-types-from-type [class ancestors type-graph]
  (some->>
   (resolve-fields class ancestors type-graph)
   (map (fn [field] (TypeGraph/get-type type-graph (:name field))))))

(defn equivalence-class-of [a b n type-graph]
  "Return the equivalence class of `a` and `b` if it exists."
  (let [min-distance 0
        max-distance (- (* 2 n) 2)
        ancestor-distance (distance-from-structural-common-ancestors a b type-graph)
        in-bounds? (fn [d] (<= min-distance d max-distance))]
    (match [n (into [] ancestor-distance)]
           [_ nil] nil
           [0 [T 0]] (list T 0)
           [1 [T (d :guard #(<= min-distance % 1))]] (list T d)
           [(_ :guard #(>= % 2)) [T (d :guard in-bounds?)]] (list T d)
           :else nil)))

(defn fields-equivalence-class-of [a b n type-graph]
  "Return the equivalence class of the types of `a` and `b`.

Lookup the types of the Field structure and compute the equivalence class via
`equivalence-class-of`."
  (let [a-type (TypeGraph/get-type type-graph (:name a))
        b-type (TypeGraph/get-type type-graph (:name b))]
    (equivalence-class-of a-type b-type n type-graph)))

(defn similarity-relation? [a b N type-graph]
  "Determine whether the two types `a` and `b` are similar up to `N`."
  (cond (or (= (types/type-form a) :interface)
            (= (types/type-form b) :interface)) false
        (= a b) true
        :else
        (let [ancestor-distance (distance-from-structural-common-ancestors a b type-graph)]
          (if (nil? ancestor-distance) false
              (let [distance (second ancestor-distance)]
                (reduce (fn [a b] (or a b))
                        false
                        (map (fn [n] (<= 0 distance n)) (range 0 (+ 1 N)))))))))

(defn fields-similarity-relation? [a b N type-graph]
  (let [a-type (TypeGraph/get-type type-graph (:name a))
        b-type (TypeGraph/get-type type-graph (:name b))]
    (similarity-relation? a-type b-type N type-graph)))

(defn- filter-unmatched-fields-transducer [groups]
  (fn [rf]
    (let [matched (into #{} (flatten (vals groups)))]
      (fn
        ;; init
        ([] (rf))
        ;; complete
        ([result]
         (rf result))
        ;; work
        ([result a]
         (if (contains? matched a)
           result
           (rf result a)))))))

(defn- filter-unmatched-correspondence-transducer [matches]
  (fn [rf]
    (let [matched (into #{} (flatten matches))]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result field]
         (if (contains? matched field)
           result
           (rf result field)))))))

(defn- partition-fields-transducer [type-graph]
  "Create a transducer that will iterate over a zip of field permutations.

The result of educing will be a map of equivalence classes for the pairs of
fields (if a match could be found)."
  (fn [rf] ;; merge? merge-with?
    ;; some state for the transducer
    (let [matched-types (volatile! #{})
          matched-as (volatile! #{})
          matched-bs (volatile! #{})]
      (fn
        ;; init
        ([] (rf))
        ;; complete
        ([result]
         (rf result))
        ;; work
        ([result [a b n]]
         (cond (or (contains? (deref matched-as) a)
                   (contains? (deref matched-bs) b)) result
               :else
               (let [class (fields-equivalence-class-of a b n type-graph)]
                 (if (nil? class)
                   result
                   (do (vswap! matched-as
                               (fn [v] (set/union v #{a})))
                       (vswap! matched-bs
                               (fn [v] (set/union v #{b})))
                       (rf result {class [[a b]]}))))))))))

(defn- field-correspondence-transducer [N type-graph]
  "Create a transducer that will iterate over a zip of field permutations.

The result of educing will be a (sub)set of ordered pairs of the input zip."
  (fn [rf] ;; into?
    ;; some state for the transducer
    (let [matched-as (volatile! #{})
          matched-bs (volatile! #{})]
      (fn
        ;; init
        ([] (rf))
        ;; complete
        ([result] (rf result))
        ;; work
        ([result [a b]]
         (cond (or (contains? (deref matched-as) a)
                   (contains? (deref matched-bs) b)) result
               :else
               (if (fields-similarity-relation? a b N type-graph)
                 (do (vswap! matched-as (fn [v] (set/union v #{a})))
                     (vswap! matched-bs (fn [v] (set/union v #{b})))
                     (rf result [[a b]]))
                 result)))))))

(defn- zip-field-permutations [as bs ns]
  "Zip the various permutations of `as`, `bs`, and `ns`."
  (for [a as
        b bs
        n ns]
    [a b n]))

(defn partition-fields-into-classes [A B N type-graph]
    "Partition field sets into equivalence classes

Returns a map containing the classes that were matched of elements in
equivalence classes, the last element will be the elements that fell into the
default equivalence class (rooted on Object)."
  (let [ancestors (get-structural-common-ancestors A B type-graph)
        a-fields (resolve-fields A ancestors type-graph)
        b-fields (resolve-fields B ancestors type-graph)
        ns (range 0 (+ N 1))
        groups (transduce (partition-fields-transducer type-graph)
                          (partial merge-with into)
                          (zip-field-permutations a-fields b-fields ns))
        unmatched (into [] (filter-unmatched-fields-transducer groups)
                        (concat a-fields b-fields))]
    (merge groups {:unmatched unmatched})))

(defn structural-correspondence-of-fields [a-fields b-fields N type-graph]
  (let [matches (transduce (field-correspondence-transducer N type-graph)
                           into
                           (for [a a-fields
                                 b b-fields] [a b]))
        unmatched (into []
                        (filter-unmatched-correspondence-transducer matches)
                        (concat a-fields b-fields))]
    {:matched-pairs matches
     :unmatched unmatched}))

(defn structural-correspondence [A B N type-graph]
  (let [ancestors (get-structural-common-ancestors A B type-graph)
        a-fields (resolve-fields A ancestors type-graph)
        b-fields (resolve-fields B ancestors type-graph)]
    (structural-correspondence-of-fields a-fields b-fields N type-graph)))

(defn structural-correspondence-wo-projection [A B N type-graph]
  (structural-correspondence-of-fields (types/type-fields A)
                                       (types/type-fields B)
                                       N
                                       type-graph))

(defn unrelated-classes? [a b type-graph]
  (let [A (types/type-name a)
        B (types/type-name b)]
    (= (distance-from-structural-common-ancestors A B type-graph) ##Inf)))
