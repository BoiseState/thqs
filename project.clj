(defproject thqs "0.1.0-SNAPSHOT"
  :description "Type Hierarchy Query System"
  :url "http://github.com/BoiseState/thqs"
  :license {:name "GPL-3.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.soot-oss/soot "4.2.1"]
                 [org.jgrapht/jgrapht-core "1.5.0"]
                 [org.jgrapht/jgrapht-io "1.5.0"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure.typed/checker.jvm "1.0.1"]
                 [org.clojure.typed/runtime.jvm "1.0.1"]
                 [org.clojure.typed/analyzer.jvm "1.0.1"]
                 [org.clojure/test.check "1.1.0"]
                 [expectations/clojure-test "1.2.1"]]
  :main edu.boisestate.cs.thqs.main.Main
  :target-path "target/%s"
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  :profiles {:uberjar {:aot :all}})
