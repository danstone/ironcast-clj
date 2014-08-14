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

(defn los? [world a b]
  (-> (take-while #(transparent-at? world %)
                  (line a b))
      last
      (= b)))

(defn player-visibility
  ([world]
   (player-visibility world 5))
  ([world range]
    (let [transparent-at? #(transparent-at? world %)]
      (->> (for [pl (players world)
                 :let [a (pos world pl)]
                 :when a
                 opaque (-> world :with-flag :opaque)
                 :let [b (pos world opaque)]
                 :when b
                 p (cons b
                         (take-while transparent-at?
                                     (line a b)))
                 :when (<= (manhattan-dist a p) range)]
             p)
           (into #{})))))

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