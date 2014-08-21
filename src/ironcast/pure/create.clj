(ns ironcast.pure.create
  (:require [ironcast.pure.attr :refer :all]
            [ironcast.pure.pos :refer :all]
            [ironcast.db :as db]
            [ironcast.util :refer :all]))

(defn next-id
  [world]
  (let [n (:gid world 0)]
    [(assoc world :gid (inc n)) n]))

(defn floor
  [world ent pt sprite]
  (-> world
      (add-flag
        ent
        :floor)
      (add-attr
        ent
        :sprite sprite)
      (try-put ent pt)))

(defn wall
  [world ent pt sprite]
  (-> world
      (add-flag ent
        :solid
        :opaque
        :wall)
      (add-attr ent
        :sprite sprite)
      (try-put ent pt)))

(defn decor
  [world ent pt sprite]
  (-> world
      (add-flag
        ent
        :decor)
      (add-attr
        ent
        :sprite sprite)
      (try-put ent pt)))

(defn creature
  [world ent pt flags & {:as attrs}]
  (-> (apply add-flag
        world
        ent
        :solid
        :creature
        flags)
      (add-attrs
        ent
        attrs)
      (try-put ent pt)))

(defn item
  [world ent flags & {:as attrs}]
  (-> (apply add-flag
             world
             ent
             :item
             flags)
      (add-attrs
        ent
        attrs)
      success))

(defn base-door
  [world ent attrs]
  (-> world
      (add-flag
        ent :door :decor)
      (add-attrs
        ent attrs)))

(defn closed-door
  [world ent pt & {:as attrs}]
  (-> (base-door world ent attrs)
      (close ent)
      (try-put ent pt)))

(defn try-create
  [world try-f & args]
  (let [[nw-a id] (next-id world)]
    (let [[nw-b success?] (apply try-f nw-a id args)]
      (if success? [nw-b id]
                   [world false]))))

(defn try-create-many
  [world try-fn coll]
  (let [[head & rest] coll]
    (if head
      (let [[new-world success?] (try-fn world head)]
        (if success?
          (recur new-world try-fn rest)
          [world false]))
      [world true])))

(defn create
  [world try-f & args]
  (first (apply try-create world try-f args)))


(def terrain-fn
  {:floor floor
   :wall wall})

(defn tiled-terrain
  [world ent db {:keys [sprite pos terrain]}]
  (let [f (get terrain-fn terrain)]
    (f world ent pos (db/find-sprite db sprite))))

(defn tiled-decor
  [world ent db {:keys [sprite pos]}]
  (decor world ent pos (db/find-sprite db sprite)))

(defn try-create-tiled
  [world db try-fn tiled-coll]
  (try-create-many world #(try-create %1 try-fn db %2) tiled-coll))

(defn try-create-world
  [db seed tiled-map]
  (try-state [world {:width (:width tiled-map)
                     :height (:height tiled-map)
                     :seed (long seed)}]
             (try-create-tiled world db tiled-terrain (:terrain tiled-map))
             (try-create-tiled world db tiled-decor (:decor tiled-map))))

