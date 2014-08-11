(ns ironcast.tiled
  (:require [clojure.set :as sets]
            [clj-tiny-grid.core :as grid]
            [ironcast
             [util :refer :all]]
            [clojure.string :as str]))


;;layers
(defn find-layer
  "find a layer in the tiled map"
  [tiled-map name]
  (find-first #(= (:name %) name) (:layers tiled-map)))


;;tilesets
(defn tileset->tile-count
  "the number of tiles in a given tileset"
  [{width :tilewidth
    height :tileheight
    img-width :imagewidth
    img-height :imageheight}]
  (* (/ img-width width) (/ img-height height)))

(defn tileset->terrain-by-index
  "derive a map of terrain-index to terrain-name for a given tileset"
  [tileset]
  (->> (map-indexed (fn [i tile] [i (:name tile)]) (:terrains tileset))
       (into {})))

(defn initial-tile
  [id]
  {:id id})

(defn tileset->terrain-by-tile-id
  "derive a map of tile-index to terrain-name for a given tileset"
  [tileset]
  (let [terr-map (tileset->terrain-by-index tileset)]
    (->> (:tiles tileset)
         (map (fn [[k {[i] :terrain}]]
                [(Integer. (.substring (str k) 1)) (terr-map i)]))
         (into {}))))

(defn initial-tiles
  [tileset]
  (map initial-tile
       (range (tileset->tile-count tileset))))

(defn assoc-terrain
  [terr-map tile]
  (assoc-if-val
    tile :terrain (terr-map (:id tile))))

(defn assoc-terrains
  [terr-map tiles]
  (map #(assoc-terrain terr-map %) tiles))

(defn add-gid
  [gid tile]
  (update-in tile [:id] + gid))

(defn add-gids
  [gid tiles]
  (map #(add-gid gid %) tiles))

(defn assoc-images
  [{img :image} tiles]
  (map #(assoc % :file img) tiles))

(defn assoc-rect
  [tw iw tile]
  (let [i (* (:id tile) tw)
        [x y] (rindmap i iw)]
    (assoc tile :rect [x (* y tw) tw tw])))

(defn assoc-rects
  [{tw :tilewidth iw :imagewidth} tiles]
  (map #(assoc-rect tw iw %) tiles))

(defn trim-file
  [file]
  (first (str/split (last (str/split file #"/")) #"\." )))

(defn trim-files
  [maps]
  (map #(update-in % [:file] trim-file) maps))

(defn tileset->tiles
  "take a tileset and return a seq of 'tiles'"
  [{gid :firstgid :as tileset}]
  (->> (initial-tiles tileset)
       (assoc-terrains (tileset->terrain-by-tile-id tileset))
       (assoc-images tileset)
       (assoc-rects tileset)
       (trim-files)
       (add-gids gid)
       (index-by :id)))


;;terrains
(def solid-terrains
  #{"wall"})

(defn terrain->walk
  [data]
  (grid/map-cell-vals (comp not solid-terrains :terrain) data))

(defn terrain->sprites
  [sprites data]
  (let [key #(select-keys % [:file :rect])
        lookup (comp sprites key)]
    (grid/map-cell-vals lookup data)))

;;tiled-map
(defn tiled-map->tiles
  [tiled-map]
  (into {} (mapcat tileset->tiles (:tilesets tiled-map))))

(defn tiled-map->terrain
  [layer-name tiled-map]
  (let [tiles (tiled-map->tiles tiled-map)
        tiles+transform #(some-> (tiles %)
                                 (update :terrain keyword))
        width (:width tiled-map)
        data (:data (find-layer tiled-map layer-name))]
    (for [[i t] (map-indexed #(vector %1 (tiles+transform %2)) data)
          :when t]
      (assoc t :pos (rindmap i width)))))

(defn objs
  "transform a obj layer into something more simple"
  [layer]
  (for [o (:objects layer)
        :let [{:keys [x y width height]} o]]
    (merge (dissoc o
                   :x :y :width :height :visible)
           {:rect (-> [x y width height]
                      (div-rectn 32)
                      int-tuple)})))


(defn terrain
  [terrains]
  (for [t terrains]
    {:sprite (select-keys t [:rect :file])
     :terrain (:terrain t)
     :pos (:pos t)}))

(defn decor
  [terrains]
  (for [t terrains]
    {:sprite (select-keys t [:rect :file])
     :pos (:pos t)}))

(defn create-map
  "create an initial map"
  [tiled-map]
  {:terrain (terrain (tiled-map->terrain "base" tiled-map))
   :decor   (decor (tiled-map->terrain "decor" tiled-map))
   :objects (objs (find-layer tiled-map "obj"))
   :width (:width tiled-map)
   :height (:height tiled-map)})
