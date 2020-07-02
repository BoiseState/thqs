(ns edu.boisestate.cs.thqs.core.similarity
  (:require [clojure.set :as set]
            [edu.boisestate.cs.thqs.core.query :as query]
            [edu.boisestate.cs.thqs.core.types :as types]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]))

(defn similarity-scores [a b type-graph]
  (let [graph (TypeGraph/graph type-graph)
        a-b-distance (query/geodesic-distance a b type-graph)
        a-ancestors (query/get-ancestor-set a type-graph)
        b-ancestors (query/get-ancestor-set b type-graph)
        a-diff (set/difference a-ancestors b-ancestors)
        b-diff (set/difference b-ancestors a-ancestors)
        ab-diff-count (+ (count a-diff) (count b-diff))]
    {:common-distance (query/distance-from-structural-common-ancestors a b type-graph)
     :normalized-difference (/ ab-diff-count (+ (count (set/union a-ancestors b-ancestors))))
     :normalized-similarity (/ (count (set/intersection a-ancestors b-ancestors))
                               (count (set/union a-ancestors b-ancestors)))
     :distance a-b-distance
     }))

(defn inclusion-exclusion-score [a b type-graph]
  (let [graph (TypeGraph/graph type-graph)
        a-ancestors (query/get-ancestor-set a graph)
        b-ancestors (query/get-ancestor-set b graph)]
    (- (count (set/union a-ancestors b-ancestors))
       (count (set/intersection a-ancestors b-ancestors)))))

(defn similarity-matrix [type-graph]
  "Compute similarity matrix of `type-graph`."
  (let [type-set (TypeGraph/type-set type-graph)
        pairs (for [x (keys type-set) y (keys type-set)] (list x y))]
    (for [[a-sym b-sym] pairs]
        (let [a (get type-set a-sym)
              b (get type-set b-sym)]
          (list a-sym b-sym (similarity-scores a b type-graph))))))

(defn structural-similarity-match? [A B level type-graph]
  "Return true or false of field matching between `A` and `B` to a particular level."
  (let [ancestors (query/get-structural-common-ancestors A B type-graph)
        a-fields (query/resolve-fields A ancestors type-graph)
        n-a-fields (count a-fields)
        b-fields (query/resolve-fields B ancestors type-graph)
        n-b-fields (count b-fields)
        total-fields (+ n-a-fields n-b-fields)
        correspondence-map (query/structural-correspondence-of-fields a-fields
                                                                      b-fields
                                                                      level
                                                                      type-graph)
        unmatched-count (count (:unmatched correspondence-map))
        matched-count (count (flatten (:matched-pairs correspondence-map)))]
    (cond (= 0 unmatched-count) [true matched-count total-fields]
          (and (< n-a-fields n-b-fields)
               (= unmatched-count
                  (- n-b-fields n-a-fields))) [true matched-count total-fields]
          (and (< n-b-fields n-a-fields)
               (= unmatched-count
                  (- n-a-fields n-b-fields))) [true matched-count total-fields]
          :else [false matched-count total-fields])))

(defn structural-similarity-match-wo-projection? [A B level type-graph]
  "Return true or false of field matching between `A` and `B` to a particular level."
  (let [ancestors (query/get-structural-common-ancestors A B type-graph)
        a-fields (types/type-fields A)
        n-a-fields (count a-fields)
        b-fields (types/type-fields B)
        n-b-fields (count b-fields)
        total-fields (+ n-a-fields n-b-fields)
        correspondence-map (query/structural-correspondence-of-fields a-fields
                                                                      b-fields
                                                                      level
                                                                      type-graph)
        unmatched-count (count (:unmatched correspondence-map))
        matched-count (count (flatten (:matched-pairs correspondence-map)))]
    (cond (= 0 unmatched-count) [true matched-count total-fields]
          (and (< n-a-fields n-b-fields)
               (= unmatched-count
                  (- n-b-fields n-a-fields))) [true matched-count total-fields]
          (and (< n-b-fields n-a-fields)
               (= unmatched-count
                  (- n-a-fields n-b-fields))) [true matched-count total-fields]
          :else [false matched-count total-fields])))
