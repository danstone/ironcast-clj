(ns ironcast.pure.act-pos
  (:require [ironcast.pure
             [act :refer :all]
             [attr :refer :all]
             [pos :refer :all]
             [move :refer [move]]]
            [ironcast.util :refer :all]
            [ironcast.pure.vis :as vis]))


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

(defmethod console-log :move
  [world action]
  (str-words "Moving" (:ent action) "to" (:to action)))

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

(defmethod console-log :open
  [_ _]
  (str-words "Opening door..."))

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

(defmethod console-log :close
  [_ _]
  (str-words "Closing door..."))

;; ATTACK
(def attack-action
  {:type :attack
   :mouse-sprite :mouse-attack
   :mouse-sprite-grey :mouse-attack-grey
   :cost 2})

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

(defmethod console-log :attack
  [world action]
  (str-words (:ent action) "Attacks" (:target action)))

(defmethod log :attack
  [world action]
  [(console-log world action)])



;; TRIP
(def trip-action
  {:type :trip
   :name "Trip"
   :cost 3
   :fatigue 10})

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

(defmethod console-log :trip
  [world action]
  (str-words (:ent action) "Trips" (:target action)))

(defmethod log :trip
  [world action]
  [(console-log world action)])

;;PICKUP

(def pickup-action
  {:type :pickup
   :name "Pickup"
   :cost 1})

(defmethod could? :pickup
  [world ent pt _]
  (and
    (item-at? world pt)
    (pos-adj? world ent pt)))

(defmethod prepare :pickup
  [world ent pt action]
  (let [items (items-at world pt)]
    (assoc action :ent ent
                  :items items)))

(defmethod try-perform :pickup
  [world action]
  (let [ent (:ent action)
        items (:items action)]
    (success
      (reduce #(-> (unput %1 %2)
                   (equip ent %2)) world items))))


;;TRAVEL

(def transition-action
  {:type :transition
   :name "Transition"})


(defmethod could? :transition
  [world ent pt _]
  (let [tra (transition-at world pt)]
    (and
      tra
      (vis/visible? world pt)
      (<= (reduce max 0 (map #(distance world %1 tra) (players world))) 10))))

(defmethod show :transition
  [world ent pt _]
  (let [trans (transition-at world pt)]
    (attr world trans :text "Travel to ???")))


(defmethod prepare :transition
  [world ent pt action]
  (assoc action
    :to (attr world (transition-at world pt) :to)))

(defmethod try-perform :transition
  [world action]
  (success world))

;; DEFAULTS

(def default-actions
  [move-action
   attack-action
   open-action])

(def other-actions
  [close-action
   open-action
   trip-action
   pickup-action
   transition-action])