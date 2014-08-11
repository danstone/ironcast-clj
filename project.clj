(defproject ironcast "0.1.0-SNAPSHOT"
  :description "Tactical Dungeoneering"
  :url "http://github.com/danstone/ironcast-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[codox "0.8.0"]
            [lein-kibit "0.0.8"]]
  :codox {:src-dir-uri "https://github.com/danstone/ironcast-clj/blob/master/"
          :src-uri-mapping {#"src" #(str "src/" (.replace % "\\" "/"))}
          :sources     ["src"]
          :src-linenum-anchor-prefix "L"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cheshire "5.3.1"]
                 [clj-tiny-grid "0.1.0-SNAPSHOT"]
                 [clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [clj-tuple "0.1.5"]
                 [amalloy/ring-buffer "1.0"]
                 [org.clojure/core.async "0.1.298.0-2a82a1-alpha"]
                 [prismatic/plumbing "0.2.0"]
                 [com.badlogicgames.gdx/gdx "1.2.1-SNAPSHOT"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.2.1-SNAPSHOT"]
                 [com.badlogicgames.gdx/gdx-platform "1.2.1-SNAPSHOT"
                  :classifier "natives-desktop"]]
  :global-vars {*warn-on-reflection* true}
  :java-source-paths ["src-java"]
  :repositories [["sonatype"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]])
