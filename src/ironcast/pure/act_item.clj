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
  (or (equipped? world ent target)
      (in-bag? world target)))

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


;;PICKUP

(def pickup-action
  {:type :pickup-item
   :name "Pickup"
   :cost 1})

(defmethod could? :pickup-item
  [world ent target _]
  (let [a (pos world ent)
        b (pos world target)]
    (when (and a b)
      (or (= a
             b)
          (adj? a b)))))

(defmethod prepare :pickup-item
  [world ent target action]
  (assoc action :ent ent
                :items [target]))

(defmethod try-perform :pickup-item
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce pickup-item world items))))

;;EQUIP
(def equip-action
  {:type :equip-item
   :name "Equip"
   :cost 1})

(defmethod could? :equip-item
  [world ent target _]
  (not (equipped? world ent target)))

(defmethod prepare :equip-item
  [world ent target action]
  (assoc action :ent ent
                :items [target]))

(defmethod try-perform :equip-item
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce #(-> (pickup-item %1 %2)
                   (equip ent %2)) world items))))


;;EQUIP L
(def equip-left-action
  {:type :equip-left-item
   :name "Equip (Left)"
   :cost 1})

(defmethod could? :equip-left-item
  [world ent target _]
  (and (hand? world target)
    (not (equipped? world ent target))))

(defmethod prepare :equip-left-item
  [world ent target action]
  (assoc action :ent ent
                :items [target]))

(defmethod try-perform :equip-left-item
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce #(-> (pickup-item %1 %2)
                   (equip-left ent %2)) world items))))

;;EQUIP R
(def equip-right-action
  {:type :equip-right-item
   :name "Equip (Right)"
   :cost 1})

(defmethod could? :equip-right-item
  [world ent target _]
  (and (hand? world target)
       (not (equipped? world ent target))))

(defmethod prepare :equip-right-item
  [world ent target action]
  (assoc action :ent ent
                :items [target]))

(defmethod try-perform :equip-right-item
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce #(-> (pickup-item %1 %2)
                   (equip-right ent %2)) world items))))

;;UNEQUIP

(def unequip-action
  {:type :unequip-item
   :name "Unequip"
   :cost 1})

(defmethod could? :unequip-item
  [world ent target _]
  (equipped? world ent target))

(defmethod prepare :unequip-item
  [world ent target action]
  (assoc action :ent ent
                :items [target]))

(defmethod try-perform :unequip-item
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce #(unequip %1 ent %2) world items))))

(def other-actions
  [equip-action
   equip-right-action
   equip-left-action
   unequip-action
   drop-action
   pickup-action])