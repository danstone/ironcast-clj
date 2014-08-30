(ns ironcast.create
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]
             [time :as time]
             [create :refer :all]]
            [ironcast.util :refer :all]
            [ironcast.state :as state]
            [clojure.set :as set]
            [ironcast.db :as db]))


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
      (merge (:attrs obj))
      (dissoc :attrs :move-goal :path)))

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
                  nw (push-obj @state/world id cooked)]
              (ref-set state/world nw)
              (alter state/gid inc)
              id))]
      (println "created... " result))))

(defn create-world!
  [tiled-map]
  (dosync
    (alter state/world time/stop-motion))
  (println "creating world..." (-> tiled-map :name (or :???) name))
  (let [world (select-keys tiled-map [:width :height :name])]
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
          removed (time/stop-motion removed)
          starts (starting-pts @state/world)
          starts (concat starts (mapcat adj starts))]
      (swap! state/db assoc-in [:worlds (:name removed)] (dissoc removed :bag))
      (dosync
        (alter state/world assoc :bag (:bag world))
        (alter state/world #(reduce (fn [world item]
                                      (-> (add-attrs world item (all removed item))
                                          (add-flags item (all-flags removed item)))) % (:bag world))))
      (doseq [[player pt] (map vector players starts)]
        (println "putting player at" pt)
        (create! (-> (assoc player
                          :pos pt
                          :type :creature)))
        (doseq [equip (concat (:equip player))]
          (create! (-> (assoc equip
                         :type :item)
                       (dissoc :pos))))))))




