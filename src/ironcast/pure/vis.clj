(ns ironcast.pure.vis
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]]
            [ironcast.util :refer :all]
            [clojure.set :as set]))

(defn visible?
  [world pt]
  (-> world :visible (get pt) boolean))

(defn explored?
  [world pt]
  (-> world :explored (get pt) boolean))

(defn add-visibility
  [world visibility]
  (assoc world :visible visibility
               :explored (set/union (:explored world #{}) visibility)))

(defn los
  [world a b]
  (take-while #(transparent-at? world %)
              (line a b)))

(defn player-los
  [world a b]
  (take-while #(explored? world %) (los world a b)))

(defn- los*?
  [world b los]
  (if (transparent-at? world b)
    (= (last los) b)
    (adj? (last los) b)))

(defn los?
  [world a b]
  (los*? b (los world a b)))

(defn player-visibility
  ([world]
   (player-visibility world 5))
  ([world range]
   (->> (for [pl (players world)
              :let [[x y :as a] (pos world pl)]
              :when a
              :let [circle (filled-circle x y range)]
              b circle
              :let [l (los world a b)]
              :when (los*? world b l)
              p (cons b l)]
          p)
        (into #{}))))

(defn visible-from
  "Find the sequence of entities visible from `pt`"
  [world entities range pt]
  (for [e entities
        :let [to (pos world e)]
        :when (and (not= pt to)
                   (<= (manhattan-dist pt to) range)
                   (los? world pt to))]
    e))

(defn visible-by
  "Find the sequence of entities visible by `entity`"
  [world ent entities]
  (visible-from world entities 5 (pos world ent)))

(defn visible-by-sorted
  "Find the sequence of entities visible by `entity` sorted by distance"
  [world ent entities]
  (->>
    (visible-by world ent entities)
    (sort-by #(distance world ent %))))