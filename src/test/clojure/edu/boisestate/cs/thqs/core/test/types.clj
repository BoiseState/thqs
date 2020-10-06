(ns edu.boisestate.cs.thqs.core.test.types
  (:use  [clojure.test])
  (:require [edu.boisestate.cs.thqs.core.types :as types]
            [edu.boisestate.cs.thqs.core.TypeGraph :as TypeGraph]
            [expectations.clojure.test :refer [expect]]))

(deftest test-core-java-type
  (expect true (types/core-java-type? 'java.lang.Object))
  (expect false (types/core-java-type? 'edu.boisestate.cs.thqs.core.TypeGraph))
  (expect false (types/core-java-type? 'com.example.java-project))
  (expect true (types/core-java-type? 'javax.swing.AbstractListModel))
  (expect true (types/core-java-type? 'com.sun.tools.attach.spi.AttachProvider))
  (expect true (types/core-java-type? 'jdk.jshell.Diag)))

(deftest test-primitive-type-predicate
  (expect true (@#'types/primitive-array? "java.lang.Object[]"))
  (expect false (@#'types/primitive-array? "java.lang.Object"))
  (expect true (@#'types/primitive-array? "int[]")))

(defn ->test-TypeGraph []
  (let [java-object (types/map->Type {:name 'java.lang.Object
                                      :package-name 'java.lang
                                      :interfaces ()
                                      :superclass nil
                                      :fields ()
                                      :form :concrete
                                      :accessor-level :public})
        A (types/map->Type {:name 'footology.A
                            :package-name 'footology
                            :interfaces '()
                            :superclass nil
                            :fields '()
                            :form :interface
                            :accessor-level :public})
        B (types/map->Type {:name 'footology.B
                            :package-name 'footology
                            :interfaces '()
                            :superclass nil
                            :fields '()
                            :form :interface
                            :accessor-level :public})
        C (types/map->Type {:name 'footology.C
                            :package-name 'footology
                            :interfaces '()
                            :superclass nil
                            :fields '()
                            :form :interface
                            :accessor-level :public})
        AbstractA (types/map->Type {:name 'footology.AbstractA
                                    :package-name 'footology
                                    :interfaces '(footology.A)
                                    :superclass 'java.lang.Object
                                    :fields '()
                                    :form :abstract
                                    :accessor-level :public})
        AbstractB (types/map->Type {:name 'footology.AbstractB
                                    :package-name 'footology
                                    :interfaces '(footology.B)
                                    :superclass 'java.lang.Object
                                    :fields '()
                                    :form :abstract
                                    :accessor-level :public})
        AbstractC (types/map->Type {:name 'footology.AbstractC
                                    :package-name 'footology
                                    :interfaces '(footology.C)
                                    :superclass 'java.lang.AbstractB
                                    :fields '()
                                    :form :abstract
                                    :accessor-level :public})
        FooA (types/map->Type {:name 'footology.FooA
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractA
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        BarA (types/map->Type {:name 'footology.BarA
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractA
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        FooB (types/map->Type {:name 'footology.FooB
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractB
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        BarB (types/map->Type {:name 'footology.BarB
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractB
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        FooC (types/map->Type {:name 'footology.FooC
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractC
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        BarC (types/map->Type {:name 'footology.BarC
                               :package-name 'footology
                               :interfaces '()
                               :superclass 'footology.AbstractC
                               :fields '()
                               :form :concrete
                               :accessor-level :public})
        type-set {'java.lang.Object java-object
                  'footology.A A
                  'footology.B B
                  'footology.C C
                  'footology.AbstractA AbstractA
                  'footology.AbstractB AbstractB
                  'footology.AbstractC AbstractC
                  'footology.FooA FooA
                  'footology.BarA BarA
                  'footology.FooB FooB
                  'footology.BarB BarB
                  'footology.FooC FooC
                  'footology.BarC BarC}]
    (TypeGraph/type-set->TypeGraph type-set)))



(defn ->java-lang-type-graph []
  "Return TypeGraph for java.lang namespace."
  (let [type-set {'java.lang.Object (types/map->Type {:name 'java.lang.Object
                                                      :package-name 'java.lang
                                                      :interfaces '()
                                                      :superclass nil
                                                      :fields '()
                                                      :form :concrete
                                                      :accessor-level :public})
                  'java.lang.CharSequence (types/map->Type {:name 'java.lang.CharSequence
                                                            :package-name 'java.lang
                                                            :interfaces '()
                                                            :superclass nil
                                                            :fields '()
                                                            :form :interface
                                                            :accessor-level :public})
                  'java.lang.Comparable (types/map->Type {:name 'java.lang.Comparable
                                                          :package-name 'java.lang
                                                          :interfaces '()
                                                          :superclass nil
                                                          :fields '()
                                                          :form :interface
                                                          :accessor-level :public})
                  'java.io.Serializable (types/map->Type {:name 'java.io.Serializable
                                                          :package-name 'java.io
                                                          :interfaces '()
                                                          :superclass nil
                                                          :fields '()
                                                          :form :interface
                                                          :accessor-level :public})
                  'java.lang.Short (types/map->Type {:name 'java.lang.Short
                                                     :package-name 'java.lang
                                                     :interfaces '(java.lang.Comparable)
                                                     :superclass 'java.lang.Number
                                                     :fields '()
                                                     :form :concrete
                                                     :accessor-level :public})
                  'java.lang.Integer (types/map->Type {:name 'java.lang.Integer
                                                       :package-name 'java.lang
                                                       :interfaces '(java.lang.Comparable)
                                                       :superclass 'java.lang.Number
                                                       :fields '()
                                                       :form :concrete
                                                       :accessor-level :public})
                  'java.lang.Character (types/map->Type {:name 'java.lang.Character
                                                         :package-name 'java.lang
                                                         :interfaces '(java.io.Serializable java.lang.Comparable)
                                                         :superclass 'java.lang.Object
                                                         :fields '()
                                                         :form :concrete
                                                         :accessor-level :public})
                  'java.lang.Double (types/map->Type {:name 'java.lang.Double
                                                      :package-name 'java.lang
                                                      :interfaces '(java.lang.Comparable)
                                                      :superclass 'java.lang.Number
                                                      :fields '()
                                                      :form :concrete
                                                      :accessor-level :public})
                  'java.lang.Boolean (types/map->Type {:name 'java.lang.Boolean
                                                       :package-name 'java.lang
                                                       :interfaces '(java.io.Serializable java.lang.Comparable)
                                                       :superclass 'java.lang.Object
                                                       :fields '()
                                                       :form :concrete
                                                       :accessor-level :public})
                  'java.lang.Float (types/map->Type {:name 'java.lang.Float
                                                     :package-name 'java.lang
                                                     :interfaces '(java.lang.Comparable)
                                                     :superclass 'java.lang.Number
                                                     :fields '()
                                                     :form :concrete
                                                     :accessor-level :public})
                  'java.lang.String (types/map->Type {:name 'java.lang.String
                                                      :package-name 'java.lang
                                                      :interfaces '(java.io.Serializable java.lang.Comparable java.lang.CharSequence)
                                                      :superclass 'java.lang.Object
                                                      :fields '()
                                                      :form :concrete
                                                      :accessor-level :public})
                  'java.lang.Long (types/map->Type {:name 'java.lang.Long
                                                    :package-name 'java.lang
                                                    :interfaces '(java.lang.Comparable)
                                                    :superclass 'java.lang.Number
                                                    :fields '()
                                                    :form :concrete
                                                    :accessor-level :public})
                  'java.lang.Byte (types/map->Type {:name 'java.lang.Byte
                                                    :package-name 'java.lang
                                                    :interfaces '(java.lang.Comparable)
                                                    :superclass 'java.lang.Number
                                                    :fields '()
                                                    :form :concrete
                                                    :accessor-level :public})
                  'java.lang.Number (types/map->Type {:name 'java.lang.Number
                                                      :package-name 'java.lang
                                                      :interfaces '(java.io.Serializable)
                                                      :superclass 'java.lang.Object
                                                      :fields '()
                                                      :form :abstract
                                                      :accessor-level :public})
                  'java.lang.Void (types/map->Type {:name 'java.lang.Void
                                                    :package-name 'java.lang
                                                    :interfaces '()
                                                    :superclass 'java.lang.Object
                                                    :fields '()
                                                    :form :concrete
                                                    :accessor-level :public})}]
    (TypeGraph/type-set->TypeGraph type-set)))



(defn ->simple-fields-TypeGraph []
  (let [type-set {'java.lang.Object (types/map->Type {:name 'java.lang.Object
                                                      :package-name 'java.lang
                                                      :interfaces ()
                                                      :superclass nil
                                                      :fields ()
                                                      :form :concrete
                                                      :accessor-level :public})
                  'A (types/map->Type {:name 'A
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'X "a" :private)
                                                     (types/->Field 'Y "b" :private)
                                                     (types/->Field 'Z "c" :private))
                                       :accessor-level :public})
                  'B (types/map->Type  {:name 'B
                                        :package-name 'default
                                        :interfaces '()
                                        :superclass 'java.lang.Object
                                        :fields (list (types/->Field 'U "d" :private)
                                                      (types/->Field 'V "e" :private)
                                                      (types/->Field 'W "f" :private))
                                        :accessor-level :public})
                  'C (types/map->Type  {:name 'C
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'U "a" :private)
                                                     (types/->Field 'W "b" :private))
                                       :accessor-level :public})
                  'D (types/map->Type {:name 'D
                                       :package-name 'disconnected
                                       :interfaces '()
                                       :superclass nil
                                       :fields ()
                                       :form :concrete
                                       :accessor-level :public})
                  'E (types/map->Type {:name 'E
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'V "a" :private)
                                                     (types/->Field 'X "b" :private))
                                       :form :concrete
                                       :accessor-level :public})
                  'F (types/map->Type {:name 'F
                                       :package-name 'default
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'Y "c" :private)
                                                     (types/->Field 'U "d" :private))
                                       :form :concrete
                                       :accessor-level :public})
                  'G (types/map->Type {:name 'G
                                       :package-name 'default
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'X "a" :private))
                                       :form :concrete
                                       :accessor-level :public})
                  'H (types/map->Type {:name 'H
                                       :package-name 'default
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'W "b" :private))
                                       :form :concrete
                                       :accessor-level :public})
                  'M (types/map->Type {:name 'M
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields (list (types/->Field 'X "b" :private)
                                                     (types/->Field 'V "a" :private))
                                       :form :concrete
                                       :accessor-level :public})
                  'U (types/map->Type {:name 'U
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'Y
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})
                  'V (types/map->Type {:name 'V
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'X
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})
                  'W (types/map->Type {:name 'W
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})
                  'X (types/map->Type {:name 'X
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'java.lang.Object
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})
                  'Y (types/map->Type {:name 'Y
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'X
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})
                  'Z (types/map->Type {:name 'Z
                                       :package-name 'default
                                       :interfaces '()
                                       :superclass 'V
                                       :fields '()
                                       :form :concrete
                                       :accessor-level :public})}]
    (TypeGraph/type-set->TypeGraph type-set)))
