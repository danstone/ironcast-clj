(ns ironcast.pure.create
  (:require [ironcast.pure.attr :refer :all]
            [ironcast.pure.pos :refer :all]
            [ironcast.db :as db]
            [ironcast.util :refer :all]
            [clojure.edn :as edn]))

(defmulti try-create-object (fn [world ent db obj] (:type obj)))

(defmethod try-create-object :default
  [world ent db obj]
  (success world))

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

(defmethod try-create-object :floor
  [world ent db obj]
  (floor world ent (:pos obj) (:sprite obj)))

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

(defmethod try-create-object :wall
  [world ent db obj]
  (wall world ent (:pos obj) (:sprite obj)))

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

(defmethod try-create-object :decor
  [world ent db obj]
  (decor world ent (:pos obj) (:sprite obj)))

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

(defn open-door
  [world ent pt & {:as attrs}]
  (-> (base-door world ent attrs)
      (open ent)
      (try-put ent pt)))

(defmethod try-create-object :door
  [world ent db obj]
  (let [{:keys [open-sprite
                closed-sprite
                pos
                open?]} obj
        open-sprite (db/find-sprite db open-sprite :door-open)
        closed-sprite (db/find-sprite db closed-sprite :door-closed)
        attrs (assoc obj
                :open-sprite open-sprite
                :closed-sprite closed-sprite)
        f (if open? open-door closed-door)]
    (apply f world ent pos (apply concat attrs))))

(defn start-pad
  [world ent pt attrs]
  (-> (add-flag world ent :start)
      (add-attrs ent attrs)
      (try-put ent pt)))

(defmethod try-create-object :start
  [world ent db obj]
  (start-pad world ent (:pos obj) obj))

(defmethod try-create-object :transition
  [world ent db obj]
  (-> (add-flag world ent :transition)
      (add-attrs ent obj)
      (try-put ent (:pos obj))))

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


(defn objectify-terrain
  [db terrain]
  [(assoc terrain
        :sprite (db/find-sprite db (:sprite terrain))
        :type (:terrain terrain))])

(defn objectify-decor
  [db decor]
  [(assoc decor
        :sprite (db/find-sprite db (:sprite decor))
        :type :decor)])

(defn objectify-obj
  [obj]
  (for [pt (rect-pts (:rect obj))]
    (assoc (update obj :type read-string)
      :pos pt)))

(defn try-create-world
  ([world db tiled-map]
   (let [objects (concat (mapcat #(objectify-terrain db %) (:terrain tiled-map))
                         (mapcat #(objectify-decor db %) (:decor tiled-map))
                         (mapcat objectify-obj (:objects tiled-map)))]
     (try-create-many
       (merge world {:width  (:width tiled-map)
                     :height (:height tiled-map)
                     :name (:name tiled-map)})
       #(try-create %1 try-create-object db %2)
       objects)))
  ([db tiled-map]
   (try-create-world {} db tiled-map)))



