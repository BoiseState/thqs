(ns edu.boisestate.cs.thqs.core.types
  (:require [clojure.spec.alpha :as s])
  (:import [soot SootClass SootField]))

(def java-package-name-regex #"^[a-zA-z][a-zA-Z0-9_]*(\.[a-zA-Z0-9_]+)*$")

(s/def ::name (s/and symbol? #(re-matches java-package-name-regex (name %))))
(s/def ::package-name ::name)
(s/def ::variable-name string?)
(s/def ::interfaces (s/coll-of ::name))
(s/def ::superclass ::name)
(s/def ::accessor-level #{:private :protected :public :package-private})
(s/def ::field (s/keys :req [::name ::variable-name ::accessor-level]
                       :opt []))
(s/def ::fields (s/coll-of ::field))
(s/def ::form #{:interface :abstract :concrete :primitive})

(s/def ::type (s/keys :req [::name]
                      :opt [::package-name ::interfaces ::superclass ::fields ::form ::accessor-level]))

(defrecord Field [name variable-name accessor-level])
(defrecord Type [name package-name interfaces superclass fields form accessor-level])

(defn type-name [type]
  "Get the name of the type."
  (:name type))
(s/fdef type-name
  :args ::type
  :ret string?)

(defn type-package-name [type]
  "Get the package-name of the type."
  (:package-name type))

(defn type-interfaces [type]
  "Get the interfaces for the type."
  (:interfaces type))

(defn type-superclass [type]
  "Get the superclass of the type."
  (:superclass type))

(defn type-fields [type]
  "Get the fields of the type."
  (:fields type))

(defn type-form [type]
  "Get the form of the type."
  (:form type))

(defn- soot-class->accessor-level  [class]
  (cond (.isPrivate class) :private
        (.isProtected class) :protected
        (.isPublic class) :public
        :else :package-private)) ;; https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html

(defn- soot-class->form  [class]
  (cond (.isInterface class) :interface
        (.isAbstract class) :abstract
        (.isConcrete class) :concrete
        :else (throw (Exception. "unable to determine soot-class form"))))

(defn- soot-field-name [field]
  (->> field
       (.getType)
       (.toQuotedString)))

(defn- soot-field->Field [field]
  "Convert SootField `field` into a field map."
  (map->Field {:name (symbol (soot-field-name field))
               :variable-name (.getName field)
               :accessor-level (soot-class->accessor-level field)
               }))

(defn- primitive-array? [type-name]
  (.endsWith type-name "[]"))

(defn soot-class->Type [class]
  "Convert a SootClass `class` into a `type` map."
  (map->Type {:name (symbol (.getName class))
              :package-name (symbol (.getPackageName class))
              :interfaces (map (fn [i] (symbol (.getName i))) (.getInterfaces class))
              :superclass (cond (.hasSuperclass class) (some->> class
                                                                (.getSuperclass)
                                                                (.getName)
                                                                (symbol))
                                :else nil)
              :fields (->> (.getFields class)
                           (remove (fn [f] (primitive-array? (soot-field-name f))))
                           (map soot-field->Field))
              :form (soot-class->form class)
              :accessor-level (soot-class->accessor-level class)
              }))



(defn primitive-type? [type]
  "Return true if type is of primitive form."
  (= (:form type) :primitive))

(defn core-java-type? [type-symbol]
  "Return true if the provided `type-symbol` is a library Java type."
  (let [type (str type-symbol)]
    (or (.startsWith type "java")
        (.startsWith type "com.sun")
        (.startsWith type "sun")
        (.startsWith type "jdk"))))



(defn ->primitive-types []
  "Construct a type-set containing only Java primitive types."
  (let [void (map->Type {:name 'void
                         :package-name nil
                         :interfaces ()
                         :superclass nil
                         :fields ()
                         :form :primitive
                         :accessor-level :package-private})
        short (map->Type {:name 'short
                          :package-name nil
                          :interfaces ()
                          :superclass nil
                          :fields ()
                          :form :primitive
                          :accessor-level :package-private})
        int (map->Type {:name 'int
                        :package-name nil
                        :interfaces ()
                        :superclass nil
                        :fields ()
                        :form :primitive
                        :accessor-level :package-private})
        char (map->Type {:name 'char
                         :package-name nil
                         :interfaces ()
                         :superclass nil
                         :fields ()
                         :form :primitive
                         :accessor-level :package-private})
        double (map->Type {:name 'double
                           :package-name nil
                           :interfaces ()
                           :superclass nil
                           :fields ()
                           :form :primitive
                           :accessor-level :package-private})
        boolean (map->Type {:name 'boolean
                            :package-name nil
                            :interfaces ()
                            :superclass nil
                            :fields ()
                            :form :primitive
                            :accessor-level :package-private})
        float (map->Type {:name 'float
                          :package-name nil
                          :interfaces ()
                          :superclass nil
                          :fields ()
                          :form :primitive
                          :accessor-level :package-private})
        long (map->Type {:name 'long
                         :package-name nil
                         :interfaces ()
                         :superclass nil
                         :fields ()
                         :form :primitive
                         :accessor-level :package-private})
        byte (map->Type {:name 'byte
                         :package-name nil
                         :interfaces ()
                         :superclass nil
                         :fields ()
                         :form :primitive
                         :accessor-level :package-private})
        type-set {'void void
                  'short short
                  'int int
                  'char char
                  'double double
                  'boolean boolean
                  'float float
                  'long long
                  'byte byte}]
    type-set))
