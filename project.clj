(defproject cg "0.1.0-SNAPSHOT"
  :description "Clojure Gaming stuff"
  :url "https://github.com/ysmolsky/cg"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.badlogicgames.gdx/gdx "1.2.0"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.2.0"]
                 [com.badlogicgames.gdx/gdx-box2d "1.2.0"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.2.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.2.0"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.2.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.2.0"
                  :classifier "natives-desktop"]

                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.jordanlewis/data.union-find "0.1.0"]
                 [play-clj "0.3.8"]
                 [quil "2.0.0"]
                 
                 ]
  :source-paths ["src" "src-launcher"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]  
  :aot [cg.core.desktop-launcher]
  :main cg.core.desktop-launcher
  :plugins [[lein-midje "3.1.1"]
            [lein-ancient "0.5.5"]
            ]  
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}})
