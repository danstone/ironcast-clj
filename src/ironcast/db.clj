(ns ironcast.db
  (:import (com.badlogic.gdx.graphics.g2d TextureRegion TextureAtlas))
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [ironcast.util :refer :all]
            [ironcast.tiled :as tiled]
            [plumbing
             [core :refer [fnk]]
             [graph :as graph]]))

(defn- spritify
  [n x y w h]
  {:name n
   :rect [x y w h]})

(defn- split-sprites
  [sprites]
  (mapv #(apply spritify %)
        (partition 5 sprites)))

(defn- texture-region
  [^TextureRegion original [x y w h]]
  (TextureRegion. original
                  (int x)
                  (int y)
                  (int w)
                  (int h)))

(declare tiles atlas maps)
(def sprites
  (fnk
    [tiles atlas]
    (for [[file sprite-vec] tiles
          :let [^TextureRegion texture (.findRegion ^TextureAtlas atlas file)]
          sprite (split-sprites sprite-vec)]
      (assoc sprite
        :file file
        :region (texture-region texture (:rect sprite))))))

(def sprite-index
  (fnk
    [sprites]
    (->> (concat
           (index-by #(select-keys % [:file :rect]) sprites)
           (index-by :name sprites))
         (into {}))))

(def tiled-maps
  (fnk [maps] (map-values tiled/create-map maps)))

(def graph
  (graph/eager-compile
    {:sprites sprites
     :sprite-index sprite-index
     :tiled-maps tiled-maps}))

(defn db
  [{:keys [atlas tiles maps fonts] :as seed}]
  (assoc (graph seed)
    :fonts fonts))

(defn find-sprite
  ([db key else]
    (-> db :sprite-index (get key else)))
  ([db key]
   (find-sprite db key nil)))

(defn find-font
  [db key]
  (-> db :fonts (get key)))

(defn find-map
  [db key]
  (-> db :tiled-maps (get key)))