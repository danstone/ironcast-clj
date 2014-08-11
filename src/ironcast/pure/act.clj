(ns ironcast.pure.act
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]
             [move :refer :all]]
            [ironcast.util :refer :all]
            [ironcast.pure.time :as time]
            [ironcast.pure.attr :as attr]))

(defmulti applies? (fn [_ _ _ action] (:type action)))

(defmulti could? (fn [_ _ _ action] (:type action)))

(defmethod applies? :default
  [world ent pt action]
  (could? world ent pt action))

(defmulti prepare (fn [_ _ _ action] (:type action)))

(defmulti try-perform (fn [_ action] (:type action)))

;;MOVE
(defn can-move?
  [world ent pt]
  (boolean
    (and (not-solid-at? world pt)
         (pos world ent))))

(defn prepare-move
  [ent pt]
  {:ent  ent
   :to   pt})

(def move-action
  {:type :move})

(defn try-perform-move
  [world action]
  (success (move world (:ent action) (:to action))))

(defmethod could? :move
  [world ent pt _]
  (can-move? world ent pt))

(defmethod prepare :move
  [_ ent pt action]
  (merge action (prepare-move ent pt)))

(defmethod try-perform :move
  [world action]
  (try-perform-move world action))

;;OPEN
(def open-action
  {:type :open
   :name "Open"
   :cost 3})

(defn can-open?
  [world ent pt]
  (boolean
    (and (pos-adj? world ent pt)
         (closed-at? world pt))))

(defn prepare-open
  [world ent pt]
  {:ent ent
   :target (door-at world pt)})

(defn try-perform-open
  [world action]
  (if-let [door (:target action)]
    (success (open world door))
    (fail world)))

(defmethod could? :open
  [world ent pt _]
  (can-open? world ent pt))

(defmethod prepare :open
  [world ent pt action]
  (merge action (prepare-open world ent pt)))

(defmethod try-perform :open
  [world action]
  (try-perform-open world action))

;;CLOSE
(def close-action
  {:type :close
   :name "Close"
   :cost 3})

(defn can-close?
  [world ent pt]
  (boolean
    (and (pos-adj? world ent pt)
         (open-at? world pt)
         (not-solid-at? world pt))))

(defn prepare-close
  [world ent pt]
  {:ent ent
   :target (door-at world pt)})

(defn try-perform-close
  [world action]
  (if-let [door (:target action)]
    (success (close world door))
    (fail world)))

(defmethod could? :close
  [world ent pt _]
  (can-close? world ent pt))

(defmethod prepare :close
  [world ent pt action]
  (merge action (prepare-close world ent pt)))

(defmethod try-perform :close
  [world action]
  (try-perform-close world action))


;; ATTACK
(def attack-action
  {:type :attack
   :mouse-sprite :mouse-attack
   :mouse-sprite-grey :mouse-attack-grey})

(defn can-attack?
  [world ent pt]
  (boolean
    (and
      (pos-adj? world ent pt)
      (enemy-of-at? world ent pt))))

(defn prepare-attack
  [world ent pt]
  {:ent ent
   :target (enemy-of-at world ent pt)})

(defn try-perform-attack
  [world action]
  (success world))

(defmethod applies? :attack
  [world ent pt _]
  (enemy-of-at? world ent pt))

(defmethod could? :attack
  [world ent pt _]
  (can-attack? world ent pt))

(defmethod prepare :attack
  [world ent pt action]
  (merge action (prepare-attack world ent pt)))

(defmethod try-perform :attack
  [world action]
  (try-perform-attack world action))

;; TRIP
(def trip-action
  {:type :trip
   :name "Trip"
   :cost 3})

(defn can-trip?
  [world ent pt]
  (can-attack? world ent pt))

(defn prepare-trip
  [world ent pt]
  {:ent    ent
   :target (enemy-of-at world ent pt)})

(defn try-perform-trip
  [world action]
  (success world))

(defmethod could? :trip
  [world ent pt _]
  (can-trip? world ent pt))

(defmethod prepare :trip
  [world ent pt action]
  (merge action (prepare-trip world ent pt)))

(defmethod try-perform :trip
  [world action]
  (try-perform-trip world action))

(def default-actions
  [move-action
   attack-action
   open-action])

(def other-actions
  [close-action
   open-action
   trip-action])

(defn can-afford?
  [world ent action]
  (let [cost (:cost action 0)
        ap (current-ap world ent)]
    (or (time/real? world)
        (<= cost ap))))

(defn can?
  [world ent pt action]
  (and
    (could? world ent pt action)
    (can-afford? world ent action)))

(defn charge-cost
  [world action]
  (if (and (time/turns? world)
           (:cost action)
           (:ent action))
    (attr/sub-ap world (:ent action) (:cost action))
    world))

(defn applicate
  [world action]
  (try-state [world world]
   (try-perform world action)
   (success (charge-cost world action))))