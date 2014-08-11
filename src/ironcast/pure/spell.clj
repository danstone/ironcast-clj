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

(defmulti cast-tile (fn [_ _ spell _] (:name spell)))
(defmulti affects? (fn [_ _ spell _] (:name spell)))

(def magic-missiles
  {:name :magic-missiles
   :text "Magic Missiles"
   :sprite :magic-missile
   :type :tile})

(defn magic-missiles-missile
  [from to caster n]
  (assoc (missile from to)
    :type :spell
    :sprite :missile-magic-bolt
    :caster caster
    :spell magic-missiles
    :times n))

(defn magic-missiles-cast
  [world caster spell [x y]]
  (let [aoe (square x y 3)
        enemies (enemies-of-in-pts world caster aoe)
        freq (->> enemies cycle (take (count aoe)) frequencies)
        from (pos world caster)
        missiles (map (fn [[target n]]
                        (magic-missiles-missile from
                                                (pos world target)
                                                caster n)) freq)]
    (reduce add-missile world missiles)))



(defmethod affects? :magic-missiles
  [world caster _ pt]
  (enemy-of-at? world caster pt))

(defmethod cast-tile :magic-missiles
  [world caster spell pt]
  (success (magic-missiles-cast world caster spell pt)))