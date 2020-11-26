(ns edu.boisestate.cs.thqs.core.test.query
  (:use  [clojure.test])
  (:require [edu.boisestate.cs.thqs.core.types :refer [->Field]]
            [edu.boisestate.cs.thqs.core.test.types :as types]
            [edu.boisestate.cs.thqs.core.query :as query]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]
            [expectations.clojure.test :refer [expect]]))

(deftest test-get-ancestors
  (let [type-graph (types/->test-TypeGraph)
        a (get (TypeGraph/type-set type-graph) 'java.lang.Object)
        b (get (TypeGraph/type-set type-graph) 'footology.A)
        c (get (TypeGraph/type-set type-graph) 'footology.AbstractA)
        d (get (TypeGraph/type-set type-graph) 'footology.FooA)]
    (expect #{(:name a)} (into #{} (map :name (query/get-ancestor-set a type-graph))))
    (expect #{(:name b)} (into #{} (map :name (query/get-ancestor-set b type-graph))))
    (expect #{(:name a)
              (:name b)
              (:name c)
              (:name d)} (into #{} (map :name (query/get-ancestor-set d type-graph))))
    (expect #{(:name a)
              (:name c)
              (:name d)} (into #{} (map :name (query/get-structural-ancestors d type-graph))))))

(deftest test-get-common-ancestors
  (let [type-graph (types/->test-TypeGraph)
        a (get (TypeGraph/type-set type-graph) 'java.lang.Object)
        b (get (TypeGraph/type-set type-graph) 'footology.A)
        c (get (TypeGraph/type-set type-graph) 'footology.B)
        d (get (TypeGraph/type-set type-graph) 'footology.AbstractA)
        e (get (TypeGraph/type-set type-graph) 'footology.AbstractB)
        f (get (TypeGraph/type-set type-graph) 'footology.FooA)
        g (get (TypeGraph/type-set type-graph) 'footology.FooB)]
    (expect #{} (into #{} (map :name (query/get-common-ancestors b a type-graph))))
    (expect #{(:name a)} (into #{} (map :name (query/get-common-ancestors d e type-graph))))
    (expect #{(:name a)} (into #{} (map :name (query/get-common-ancestors f g type-graph))))))

(deftest test-field-type-mapping
  (let [type-graph (types/->simple-fields-TypeGraph)]
    (expect '() (query/get-field-types-from-type (TypeGraph/get-type type-graph 'X) '() type-graph))
    (expect '((X Y Z) (map :name (query/get-field-types-from-type (TypeGraph/get-type type-graph 'A) '() type-graph))))))

(deftest test-field-mappings
  (let [type-graph (types/->simple-fields-TypeGraph)
        object (TypeGraph/get-type type-graph 'java.lang.Object)
        A (TypeGraph/get-type type-graph 'A)
        B (TypeGraph/get-type type-graph 'B)
        E (TypeGraph/get-type type-graph 'E)
        F (TypeGraph/get-type type-graph 'F)
        G (TypeGraph/get-type type-graph 'G)
        H (TypeGraph/get-type type-graph 'H)
        M (TypeGraph/get-type type-graph 'M)
        ->field (fn [T n] (->Field T n :private))]
    (expect {:unmatched [(->field 'X "a") (->field 'W "b")]}
            (query/partition-fields-into-classes G H 2 type-graph))
    (expect {'(X 1) [[(->field 'X "b") (->field 'Y "c")]]
             '(X 3) [[(->field 'V "a") (->field 'U "d")]] :unmatched []}
            (query/partition-fields-into-classes M F 3 type-graph))
    (expect {'(X 2) [[(->field 'V "a") (->field 'Y "c")]
                     [(->field 'X "b") (->field 'U "d")]]
             :unmatched []}
            (query/partition-fields-into-classes E F 3 type-graph))
    (expect {:unmatched [(->field 'X "a")
                         (->field 'Y "b")
                         (->field 'Z "c")
                         (->field 'W "b")]}
            (query/partition-fields-into-classes A H 3 type-graph))
    (expect {'(X 1) [[(->field 'X "a") (->field 'V "a")]
                     [(->field 'Y "b") (->field 'X "b")]]
             :unmatched [(->field 'Z "c")]}
            (query/partition-fields-into-classes A E 3 type-graph))
    (expect {'(X 2) [[(->field 'X "a") (->field 'U "d")]
                     [(->field 'Y "b") (->field 'V "e")]]
             :unmatched [(->field 'Z "c")  (->field 'W "f")]}
            (query/partition-fields-into-classes A B 3 type-graph))))

(deftest test-field-correspondence
  (let [type-graph (types/->simple-fields-TypeGraph)
        object (TypeGraph/get-type type-graph 'java.lang.Object)
        A (TypeGraph/get-type type-graph 'A)
        B (TypeGraph/get-type type-graph 'B)
        E (TypeGraph/get-type type-graph 'E)
        F (TypeGraph/get-type type-graph 'F)
        G (TypeGraph/get-type type-graph 'G)
        H (TypeGraph/get-type type-graph 'H)
        M (TypeGraph/get-type type-graph 'M)
        U (TypeGraph/get-type type-graph 'U)
        V (TypeGraph/get-type type-graph 'V)
        ->field (fn [T n] (->Field T n :private))]
    (expect {:matched-pairs []
             :unmatched []}
            (query/structural-correspondence U U 0 type-graph))
    (expect {:matched-pairs []
             :unmatched []}
            (query/structural-correspondence U V 1 type-graph))
    (expect {:matched-pairs []
             :unmatched [(->field 'X "a") (->field 'W "b")]}
            (query/structural-correspondence G H 2 type-graph))
    (expect {:matched-pairs [[(->field 'X "b") (->field 'Y "c")]
                             [(->field 'V "a") (->field 'U "d")]]
             :unmatched []}
            (query/structural-correspondence M F 3 type-graph))
    (expect {:matched-pairs [[(->field 'V "a") (->field 'Y "c")]
                             [(->field 'X "b") (->field 'U "d")]]
             :unmatched []}
            (query/structural-correspondence E F 3 type-graph))
    (expect {:matched-pairs []
             :unmatched [(->field 'X "a")
                         (->field 'Y "b")
                         (->field 'Z "c")
                         (->field 'W "b")]}
            (query/structural-correspondence A H 3 type-graph))
    (expect {:matched-pairs [[(->field 'X "a") (->field 'V "a")]
                             [(->field 'Y "b") (->field 'X "b")]]
             :unmatched [(->field 'Z "c")]}
            (query/structural-correspondence A E 3 type-graph))
    (expect {:matched-pairs [[(->field 'X "a") (->field 'U "d")]
                             [(->field 'Y "b") (->field 'V "e")]]
             :unmatched [(->field 'Z "c")  (->field 'W "f")]}
            (query/structural-correspondence A B 3 type-graph))
    (expect {:matched-pairs [[(->field 'X "a") (->field 'X "a")]
                             [(->field 'Y "b") (->field 'Y "b")]
                             [(->field 'Z "c") (->field 'Z "c")]]
             :unmatched []}
            (query/structural-correspondence A A 0 type-graph))))

(deftest test-distance-calculations
  (let [type-graph (types/->simple-fields-TypeGraph)]
    (expect ##Inf
            (query/distance-from-common-ancestor (TypeGraph/get-type type-graph 'W)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 (TypeGraph/get-type type-graph 'java.lang.Object)
                                                 type-graph))
    (expect ##Inf
            (query/distance-from-common-ancestor (TypeGraph/get-type type-graph 'W)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 nil
                                                 type-graph))
    (expect 0
            (query/distance-from-common-ancestor (TypeGraph/get-type type-graph 'X)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 type-graph))
    (expect 2
            (query/distance-from-common-ancestor (TypeGraph/get-type type-graph 'V)
                                                 (TypeGraph/get-type type-graph 'Y)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 type-graph))
    (expect 4
            (query/distance-from-common-ancestor (TypeGraph/get-type type-graph 'Z)
                                                 (TypeGraph/get-type type-graph 'U)
                                                 (TypeGraph/get-type type-graph 'X)
                                                 type-graph))
    (expect (list 'java.lang.Object ##Inf)
            (query/distance-from-structural-common-ancestors (TypeGraph/get-type type-graph 'X)
                                                             (TypeGraph/get-type type-graph 'W)
                                                             type-graph))
    (expect nil
            (query/distance-from-structural-common-ancestors (TypeGraph/get-type type-graph 'X)
                                                             (TypeGraph/get-type type-graph 'D)
                                                             type-graph))
    (expect (list 'X 0)
            (query/distance-from-structural-common-ancestors (TypeGraph/get-type type-graph 'X)
                                                             (TypeGraph/get-type type-graph 'X)
                                                             type-graph))
    (expect (list 'X 2)
            (query/distance-from-structural-common-ancestors (TypeGraph/get-type type-graph 'V)
                                                             (TypeGraph/get-type type-graph 'Y)
                                                             type-graph))
    (expect (list 'X 4)
            (query/distance-from-structural-common-ancestors (TypeGraph/get-type type-graph 'Z)
                                                             (TypeGraph/get-type type-graph 'U)
                                                             type-graph))))

(deftest test-zip-field-permutations
  (expect '([a b c]) (@#'query/zip-field-permutations '(a) '(b) '(c)))
  (expect '([a c 0] [a c 1] [a d 0] [a d 1] [b c 0] [b c 1] [b d 0] [b d 1])
          (@#'query/zip-field-permutations '(a b) '(c d) (range 0 2))))

(deftest test-equivalence-class-of
  (let [type-graph (types/->simple-fields-TypeGraph)
        object (TypeGraph/get-type type-graph 'java.lang.Object)
        U (TypeGraph/get-type type-graph 'U)
        V (TypeGraph/get-type type-graph 'V)
        W (TypeGraph/get-type type-graph 'W)
        X (TypeGraph/get-type type-graph 'X)
        Y (TypeGraph/get-type type-graph 'Y)
        Z (TypeGraph/get-type type-graph 'Z)]
    (expect nil (query/equivalence-class-of X X -1 type-graph))
    (expect '(X 0) (query/equivalence-class-of X X 0 type-graph))
    (expect '(X 0) (query/equivalence-class-of X X 1 type-graph))
    (expect '(X 2) (query/equivalence-class-of V Y 2 type-graph))
    (expect '(V 1) (query/equivalence-class-of Z V 1 type-graph))
    (expect nil (query/equivalence-class-of Z U 2 type-graph))
    (expect '(X 4) (query/equivalence-class-of Z U 3 type-graph))
    (expect nil (query/equivalence-class-of X W 1 type-graph))
    (expect nil (query/equivalence-class-of U W 1 type-graph))
    (expect nil (query/equivalence-class-of V U 1 type-graph))
    (expect '(X 3) (query/equivalence-class-of V U 3 type-graph))))

(deftest test-similarity-relation-decider
  (let [type-graph (types/->simple-fields-TypeGraph)
        object (TypeGraph/get-type type-graph 'java.lang.Object)
        U (TypeGraph/get-type type-graph 'U)
        V (TypeGraph/get-type type-graph 'V)
        W (TypeGraph/get-type type-graph 'W)
        X (TypeGraph/get-type type-graph 'X)
        Y (TypeGraph/get-type type-graph 'Y)
        Z (TypeGraph/get-type type-graph 'Z)]
    (expect true (query/similarity-relation? X X 0 type-graph))
    (expect true (query/similarity-relation? X X 1 type-graph))
    (expect true (query/similarity-relation? V Y 2 type-graph))
    (expect true (query/similarity-relation? Z V 1 type-graph))
    (expect false (query/similarity-relation? Z U 2 type-graph))
    (expect true (query/similarity-relation? Z U 4 type-graph))
    (expect false (query/similarity-relation? X W 1 type-graph))
    (expect false (query/similarity-relation? U W 1 type-graph))
    (expect false (query/similarity-relation? V U 1 type-graph))
    (expect true (query/similarity-relation? V U 3 type-graph))))

(deftest test-unrelated-classes?
  (let [type-graph (types/->simple-fields-TypeGraph)
        object (TypeGraph/get-type type-graph 'java.lang.Object)
        A (TypeGraph/get-type type-graph 'A)
        B (TypeGraph/get-type type-graph 'B)
        E (TypeGraph/get-type type-graph 'E)
        F (TypeGraph/get-type type-graph 'F)
        G (TypeGraph/get-type type-graph 'G)
        H (TypeGraph/get-type type-graph 'H)
        M (TypeGraph/get-type type-graph 'M)
        U (TypeGraph/get-type type-graph 'U)
        V (TypeGraph/get-type type-graph 'V)]
    (expect true (query/unrelated-classes? A B type-graph))))
