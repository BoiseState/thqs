(ns edu.boisestate.cs.thqs.core.test.similarity
  (:use  [clojure.test])
  (:require [edu.boisestate.cs.thqs.core.test.types :as types]
            [edu.boisestate.cs.thqs.core.similarity :as sim]
            [edu.boisestate.cs.thqs.core.query :as query]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]
            [expectations.clojure.test :refer [expect]]))

(deftest test-structural-similarity-match
  (let [type-graph (types/->simple-fields-TypeGraph)
        E (TypeGraph/get-type type-graph 'E)
        F (TypeGraph/get-type type-graph 'F)
        M (TypeGraph/get-type type-graph 'M)]
    (expect [true 4 4] (sim/structural-similarity-match? M F 3 type-graph))
    (expect [false 2 4] (sim/structural-similarity-match? M F 2 type-graph))
    (expect [true 4 4] (sim/structural-similarity-match? E F 2 type-graph))
    (expect [false 2 4] (sim/structural-similarity-match? E F 1 type-graph))))
