(ns ironcast.create
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]]
            [ironcast.util :refer :all]
            [ironcast.state :as state]
            [clojure.set :as set]
            [ironcast.db :as db]
            [ironcast.pure.time :as time]))

(defn create
  [world ent flags attrs]
  (-> world
      (add-flags ent flags)
      (add-attrs ent attrs)))

(defn try-create-put
  [world ent flags attrs]
  (if (:pos attrs)
    (-> (create world ent flags (dissoc attrs :pos))
        (try-put ent (:pos attrs)))
    (fail world)))

(defmulti try-create-obj (fn [world ent obj]
                           (:type obj)))

(defmethod try-create-obj :default
  [world _ obj]
  (success world))

(defmethod try-create-obj :floor
  [world ent obj]
  (try-create-put world ent #{:floor} obj))

(defmethod try-create-obj :wall
  [world ent obj]
  (try-create-put world ent #{:wall :solid :opaque} obj))

(defmethod try-create-obj :decor
  [world ent obj]
  (try-create-put world ent #{:decor} obj))

(defmethod try-create-obj :creature
  [world ent obj]
  (try-create-put world ent (set/union #{:solid :creature}
                                   (set (:flags obj)))
              (dissoc obj :flags)))

(defmethod try-create-obj :item
  [world ent obj]
  (let [flags (set/union #{:item}
                         (set (:flags obj)))
        attr (dissoc obj :flags)]
    (if (:pos obj)
      (try-create-put world ent flags attr)
      (success (create world ent flags attr)))))

(defmethod try-create-obj :door
  [world ent obj]
  (let [open? (:open? obj)
        f (if open? open close)]
    (-> (create world ent
                #{:door :decor}
                obj)
        (f ent)
        (try-put ent (:pos obj)))))

(defmethod try-create-obj :start
  [world ent obj]
  (try-create-put world ent #{:start} obj))

(defmethod try-create-obj :transition
  [world ent obj]
  (try-create-put world ent #{:transition} obj))


(defn objectify-terrain
  [terrain]
  [(assoc terrain
     :type (:terrain terrain))])

(defn objectify-decor
  [decor]
  [(assoc decor
     :type :decor)])

(defn objectify-obj
  [obj]
  (for [pt (rect-pts (:rect obj))]
    (assoc (update obj :type read-string)
      :pos pt)))

(defmulti cook-obj (fn [db obj] (:type obj)))

(defmethod cook-obj :default
  [_ obj] obj)

(defmethod cook-obj :floor
  [db obj]
  (update obj :sprite #(db/find-sprite db % %)))

(defmethod cook-obj :wall
  [db obj]
  (update obj :sprite #(db/find-sprite db % %)))

(defmethod cook-obj :decor
  [db obj]
  (update obj :sprite #(db/find-sprite db % %)))

(defmethod cook-obj :door
  [db obj]
  (-> obj
      (update :open-sprite #(db/find-sprite db % %))
      (update :closed-sprite #(db/find-sprite db % %))))

(defmethod cook-obj :item
  [db obj]
  (-> obj
      (update :sprite #(db/find-sprite db % %))
      (update :equip-sprite #(db/find-sprite db % %))
      (dissoc :attrs :equip)
      (merge (:attrs obj))))

(defmethod cook-obj :creature
  [db obj]
  (-> obj
      (update :sprite #(db/find-sprite db % %))
      (dissoc :attrs :equip)
      (merge (:attrs obj))))

(defn create!
  [obj]
  (println "cooking entity... ")
  (let [cooked (cook-obj @state/db obj)
        name (-> cooked :type (or :???) name)]
    (println "cooked..." name)
    (println "creating..." name)
    (let [result
          (dosync
            (let [id (:ent obj @state/gid)
                  [nw success?] (try-create-obj @state/world id cooked)]
              (when success?
                (ref-set state/world nw)
                (alter state/gid inc)
                id)))]
      (if result
        (println "created... " result)
        (println "failed!")))))

(defn create-world!
  [tiled-map]
  (dosync
    (alter state/world time/stop-motion))
  (println "creating world..." (-> tiled-map :name (or :???) name))
  (let [world {:width  (:width tiled-map)
               :height (:height tiled-map)
               :name   (:name tiled-map)}]
    (dosync
      (ref-set state/world world))
    (println "created world..."  (:width world) "by" (:height world)))
  (let [objects (concat (mapcat objectify-terrain (:terrain tiled-map))
                        (mapcat objectify-decor (:decor tiled-map))
                        (mapcat objectify-obj (:objects tiled-map)))]
    (println "creating..." (count objects) "objects")
    (doseq [o objects]
      (create! o))))

(defn remove-all-players
  [world]
  (let [players (players world)
        f (fn [world ent]
            (-> (rem-all-attrs world ent)
                (rem-all-flags ent)
                (unput ent)))]
    (reduce f world players)))

(defn snapshot
  [world ent]
  {:ent ent
   :attrs (all world ent)
   :flags (all-flags world ent)
   :equip (map #(snapshot world %) (equipped world ent))})

(defn transition-to!
  [to]
  (println "transitioning to..." (name to))
  (let [db @state/db
        world @state/world
        new (-> db :worlds (get to))]
    (if new
      (dosync (ref-set state/world new))
      (create-world! (-> db :tiled-maps (get to))))
    (let [players (map #(snapshot world %) (players world))
          removed (reduce clear-equipped world players)
          removed (remove-all-players removed)
          starts (starting-pts @state/world)]
      (swap! state/db assoc-in [:worlds (:name removed)] removed)
      (doseq [[player pt] (map vector players starts)]
        (println "putting player at" pt)
        (create! (-> (assoc player
                          :pos pt
                          :type :creature)))
        (doseq [equip (:equip player)]
          (create! (-> (assoc equip
                         :type :item)
                       (dissoc :pos))))))))




