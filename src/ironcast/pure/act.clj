(ns ironcast.pure.act
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]
             [move :refer :all]]
            [ironcast.util :refer :all]
            [ironcast.pure.time :as time]
            [ironcast.pure.attr :as attr]
            [ironcast.pure.vis :as vis]))

(defmulti applies? (fn [_ _ _ action] (:type action)))

(defmulti could? (fn [_ _ _ action] (:type action)))

(defmethod applies? :default
  [world ent other action]
  (could? world ent other action))

(defmulti aoe (fn [_ _ _ action] (:type action)))
(defmethod aoe :default
  [world ent pt action]
  (list pt))

(defmulti prepare (fn [_ _ _ action] (:type action)))

(defmethod prepare :default
  [world ent other action]
  action)

(defmulti try-perform (fn [_ action] (:type action)))

(defmethod try-perform :default
  [world _]
  (success world))

(defmulti console-log (fn [_ action] (:type action)))

(defmethod console-log :default
  [world action]
  nil)

(defmulti log (fn [world action] (:type action)))

(defmethod log :default
  [world action]
  nil)

(defmulti world-text (fn [world action] (:type action)))

(defmethod world-text :default
  [world action]
  nil)

(defmulti show (fn [world ent other action] (:type action)))

(defmethod show :default
  [_ _ _ {:keys [name text cost]}]
  (str (or text name) (when cost (str " (" cost ")"))))

(defmulti descr (fn [world ent other action] (:type action)))

(defmethod descr :default
  [_ _ _ action]
  [(str-words (:type action "???") (:cost action "???") "AP")])


(defn text-on-ent
  [world action text & {:keys [color]}]
  {:pos (pos world (:ent action))
   :text text
   :time 0.0
   :color (or color :white)})

(defn text-on-target
  [world action text & {:keys [color]}]
  {:pos (pos world (:target action))
   :text text
   :time 0.0
   :color (or color :white)})

(defmethod world-text :attack
  [world action]
  [(text-on-target
     world action "1" :color :red)])

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

(defn charge-fatigue
  [world action]
  (if (and (:fatigue action)
           (:ent action))
    (attr/add-fatigue world (:ent action) (:fatigue action))
    world))

(defn applicate
  [world action]
  (try-state [world world]
   (try-perform world action)
   (success (-> world
                (charge-cost action)
                (charge-fatigue action)))))