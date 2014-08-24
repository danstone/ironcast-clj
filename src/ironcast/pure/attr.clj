(ns ironcast.pure.attr
  (:require [ironcast.util :refer :all]
            [clojure.set :as set]
            [clj-tuple :refer [tuple]]))

(defn has-flag?
  [world ent flag]
  (-> world :flags (get ent) (get flag) boolean))

(defn add-flag
  ([world ent flag & flags]
   (reduce #(add-flag %1 ent %2)
           (add-flag world ent flag)
           flags))
  ([world ent flag]
    (-> (update-in world [:flags ent] set-conj flag)
        (update-in [:with-flag flag] set-conj ent))))

(defn rem-flag
  ([world ent flag & flags]
   (reduce #(rem-flag %1 ent %2)
           (rem-flag world ent flag)
           flags))
  ([world ent flag]
    (-> (update-in world [:flags ent] disj flag)
        (update-in [:with-flag flag] disj ent))))

(defn rem-all-flags
  [world ent]
  (let [flags (-> world :flags (get ent))]
    (apply rem-flag world ent flags)))

(defn clear-flag
  [world flag]
  (let [ents (-> world :with-flag (get flag))]
    (reduce #(rem-flag %1 %2 flag) world ents)))

(defn all-flags
  [world ent]
  (-> world :flags (get ent)))

(defn all
  [world ent]
  (-> world :attr (get ent)))

(defn attr
  ([world ent att]
   (attr world ent att nil))
  ([world ent att else]
   (-> world :attr (get ent) (get att) (or else))))

(defn add-attr
  [world ent attr val]
  (-> (assoc-in world [:attr ent attr] val)
      (update-in [:with-attr attr] set-conj ent)))

(defn add-attrs
  [world ent attr-map]
  (reduce (fn [w [k v]] (add-attr w ent k v)) world attr-map))

(defn update-attr
  [world ent attr f & args]
  (apply update-in world [:attr ent attr] f args))

(defn rem-attr
  [world ent attr]
  (-> (dissoc-in world [:attr ent attr])
      (update-in [:with-attr attr] disj ent)))

(defn rem-all-attrs
  [world ent]
  (let [attrs (-> world :attr (get ent) keys)]
    (reduce #(rem-attr %1 ent %2) world attrs)))

(defn clear-attr
  "Clear the attribute from all known entities
   the new world will have the same entities, but all instances
   of the attribute will be removed."
  ([world attr]
   (let [all (-> world :with-attr (get attr))]
     (reduce #(rem-attr %1 %2 attr) world all)))
  ([world attr & attrs]
   (reduce clear-attr world (cons attr attrs))))

(defn solids
  [world]
  (-> world :with-flag :solid))

(defn solid?
  [world ent]
  (has-flag? world ent :solid))

(defn not-solid?
  [world ent]
  (not (solid? world ent)))

(defn solidify
  [world ent]
  (add-flag world ent :solid))

(defn unsolidify
  [world ent]
  (rem-flag world ent :solid))

(defn opaque?
  [world ent]
  (has-flag? world ent :opaque))

(defn transparent?
  [world ent]
  (not (opaque? world ent)))

(defn decor?
  [world ent]
  (has-flag? world ent :decor))

(defn creature?
  [world ent]
  (has-flag? world ent :creature))

(defn creatures
  [world]
  (-> world :with-flag :creature))

(defn player?
  [world ent]
  (has-flag? world ent :player))

(defn players
  [world]
  (-> world :with-flag :player))

(defn sorted-players
  [world]
  (sort (players world)))

(defn selectable?
  [world ent]
  (player? world ent))

(defn selected?
  [world ent]
  (has-flag? world ent :selected))

(defn- just-select
  [world ent]
  (add-flag world ent :selected))

(defn try-select
  [world ent]
  (if (selectable? world ent)
    (tuple (just-select world ent) true)
    (tuple world false)))

(defn select
  [world ent]
  (first (try-select world ent)))

(defn selected
  [world]
  (-> world :with-flag :selected))

(defn first-selected
  [world]
  (first (selected world)))

(defn unselect
  [world ent]
  (rem-flag world ent :selected))

(defn unselect-all
  [world]
  (clear-flag world :selected))

(defn select-only
  [world ent]
  (-> (unselect-all world)
      (select ent)))

(defn enemy?
  [world ent]
  (has-flag? world ent :enemy))

(defn enemies
  [world]
  (-> world :with-flag :enemy))

(defn ally-of?
  [world ent other]
  (or (= (player? world ent)
         (player? world other))
      (= (enemy? world ent)
         (enemy? world other))))


(defn allies-of
  [world ent]
  (filter #(and (not= % ent) (ally-of? world ent %)) (creatures world)))

(defn enemy-of?
  [world ent other]
  (and (creature? world other)
       (not (ally-of? world ent other))))

(defn enemies-of
  [world ent]
  (filter #(and (not= % ent) (enemy-of? world ent %)) (creatures world)))

(defn ai?
  [world ent]
  (has-flag? world ent :ai))

(defn ai
  [world]
  (-> world :with-flag :ai))

(defn door?
  [world ent]
  (has-flag? world ent :door))

(defn open?
  [world ent]
  (has-flag? world ent :open))

(defn closed?
  [world ent]
  (and
    (door? world ent)
    (not (open? world ent))))

(defn open
  [world ent]
  (-> (add-flag world ent :open)
      (rem-flag ent :solid :opaque)
      (add-attr ent
        :sprite (attr world ent :open-sprite))))

(defn close
  [world ent]
  (-> (rem-flag world ent :open)
      (add-flag ent :solid :opaque)
      (add-attr ent
        :sprite (attr world ent :closed-sprite))))

(defn unequip
  [world ent item]
  (-> (update-attr world ent disj item)
      (rem-attr item :on)))

(defn equip
  [world ent item]
  (-> (update-attr world ent :equip set-conj item)
      (add-attr item :on ent)))

(def default-hp (tuple 10 10))

(defn hp
  [world ent]
  (attr world ent :hp default-hp))

(defn current-hp
  [world ent]
  (first (hp world ent)))

(defn max-hp
  [world ent]
  (second (hp world ent)))

(defn update-hp
  [world ent f & args]
  (let [hp (hp world ent)]
    (add-attr world ent :hp (apply f hp args))))

(defn update-current-hp
  [world ent f & args]
  (update-hp world ent
             #(tuple (apply f (first %) args)
                     (second %))))

(defn add-hp
  [world ent amount]
  (update-current-hp world ent + amount))

(defn sub-hp
  [world ent amount]
  (update-current-hp world ent - amount))

(def default-ap (tuple 5 5))

(defn ap
  [world ent]
  (attr world ent :ap default-ap))

(defn current-ap
  [world ent]
  (first (ap world ent)))

(defn max-ap
  [world ent]
  (second (ap world ent)))

(defn update-ap
  [world ent f & args]
  (let [ap (ap world ent)]
    (add-attr world ent :ap (apply f ap args))))

(defn update-current-ap
  [world ent f & args]
  (update-ap world ent
             #(tuple (apply f (first %) args)
                     (second %))))

(defn add-ap
  [world ent amount]
  (update-current-ap world ent + amount))

(defn sub-ap
  [world ent amount]
  (update-current-ap world ent - amount))

(defn refresh-ap
  [world ent]
  (let [[_ max] (ap world ent)]
    (add-attr world ent :ap (tuple max max))))

(defn refresh-player-aps
  [world]
  (reduce refresh-ap world (players world)))

(defn refresh-enemy-aps
  [world]
  (reduce refresh-ap world (enemies world)))

(defn fatigue
  [world ent]
  (attr world ent :fatigue 0))

(defn add-fatigue
  [world ent amount]
  (update-attr world ent :fatigue #(+ (or % 0) amount)))


(defn path-sprite
  [world ent total]
  (let [ap (current-ap world ent)]
    (cond
      (<= total (/ ap 2)) :green-flag
      (<= total ap) :yellow-flag
      :otherwise :red-flag)))
