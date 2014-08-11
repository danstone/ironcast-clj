(ns ironcast.io
  (:import (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx.graphics.g2d TextureAtlas BitmapFont))
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [ironcast.util :refer :all]))


(defn atlas
  [file]
  (println "loading atlas..." file)
  (TextureAtlas. (FileHandle. (str "resources/tiles/" file))))


(defn json
  [file]
  (println "loading json..." file)
  (json/parse-string (slurp file) true))

(defn edn
  [file]
  (println "loading edn..." file)
  (edn/read-string (slurp file)))

(defn maps
  []
  (map-values json (edn "resources/maps.edn")))

(defn tiles
  []
  (edn "resources/tiles.edn"))

(defn fonts
  []
  {:default (BitmapFont.)})