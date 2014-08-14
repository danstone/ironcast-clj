(ns ironcast.pure.spell
  (:require [ironcast.util :refer :all]
            [ironcast.pure
             [act :refer :all]
             [pos :refer :all]
             [effect :refer :all]]))

(def types
  #{:tile
    :single
    :self
    :item})

(defn single?
  [spell]
  (= (:spell-type spell) :single))

(defn tile?
  [spell]
  (= (:spell-type spell) :tile))

;;MAGIC MISSILES

(def magic-missiles
  {:name :magic-missiles
   :text "Magic Missiles"
   :sprite :magic-missile
   :type :magic-missiles
   :spell-type :tile
   :cost 3})

(defn magic-missiles-missile
  [world from target n]
  (assoc (missile from (pos world target))
    :target target
    :type :magic-missiles-strike
    :sprite :missile-magic-bolt
    :spell magic-missiles
    :times n))

(defmethod applies? :magic-missiles
  [world caster pt _]
  (enemy-of-at? world caster pt))

(defmethod could? :magic-missiles
  [world caster pt _]
  true)

(defmethod aoe :magic-missiles
  [world caster [x y] spell]
  (square x y 3))

(defmethod prepare :magic-missiles
  [world caster pt spell]
  (let [aoe (aoe world caster pt spell)
        enemies (enemies-of-in-pts world caster aoe)
        freq (->> enemies cycle (take (count aoe)) frequencies)
        from (pos world caster)
        missiles (map (fn [[target n]]
                        (magic-missiles-missile world
                                                from
                                                target
                                                n)) freq)]
    (assoc spell
      :missiles missiles
      :ent caster)))

(defmethod try-perform :magic-missiles
  [world spell]
  (success (reduce add-missile world (:missiles spell))))

(defmethod console-log :magic-missiles
  [world spell]
  "Cast Magic Missiles")

(defmethod console-log :magic-missiles-strike
  [world missile]
  (str-words "Magic Missiles hit" (:times missile) "times"))

(defmethod world-text :magic-missiles
  [world spell]
  [(text-on-ent
      world spell "*Magic Missiles*")])

(defmethod world-text :magic-missiles-strike
  [world spell]
  [(text-on-target
     world spell (str (:times spell))
      :color :light-blue)])

(defmethod descr :magic-missiles
  [world caster pt spell]
  [[:light-yellow (str "Magic Missiles (" (:cost spell "???") " AP)")]
   "- Fires several missiles in area of effect"
   "- One missile per cell in area"
   "- Area of effect increases with experience"
   "- Excess missiles will strike closest enemy"
   [:light-green "- Each missile does 1d6 physical damage"]
   [:light-blue "- There is no resistence check against this spell"]
   [:light-blue "- There is no persistence check against this spell"]])

;;SLEEP TOUCH

(def sleep-touch
  {:name :sleep-touch
   :text "Sleep Touch"
   :sprite :sleep
   :type :sleep-touch
   :spell-type :single
   :cost 3})

(defmethod could? :sleep-touch
  [world caster pt _]
  (and
    (pos-adj? world caster pt)
    (enemy-of-at? world caster pt)))

(defmethod prepare :sleep-touch
  [world caster pt spell]
  (assoc spell :ent caster
               :target (enemy-of-at world caster pt)))

(defmethod try-perform :sleep-touch
  [world spell]
  (success world))

(defmethod console-log :sleep-touch
  [world spell]
  "Cast Sleep Touch")

(defmethod world-text :sleep-touch
  [world spell]
  [(text-on-ent
     world spell "*Sleep Touch*")
   (text-on-target
     world spell "zzz"
     :color :light-green)])


