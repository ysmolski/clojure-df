(defproject cg "0.1.0-SNAPSHOT"
  :description "Clojure Gaming stuff"
  :url "https://github.com/ysmolsky/cg"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main cg.core
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.0.0"]
                 [org.jordanlewis/data.union-find "0.1.0"]
                 [midje "1.6.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ]
  :profiles {:dev {:dependencies []}})
