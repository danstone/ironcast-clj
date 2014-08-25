(ns ironcast.pure.act-item
  (:require [ironcast.pure
             [act :refer :all]
             [attr :refer :all]
             [pos :refer :all]]
            [ironcast.util :refer :all]))


(def drop-action
  {:type :drop
   :name "Drop"})

(defmethod could? :drop
  [world ent target action]
  (equipped? world ent target))

(defmethod prepare :drop
  [world ent target action]
  (assoc action
    :ent ent
    :item target))

(defmethod try-perform :drop
  [world action]
  (let [{:keys [ent item]} action]
    (if (and ent item)
      (success (drop-item world ent item))
      (fail world))))

(def other-actions
  [drop-action])