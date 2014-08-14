(ns ironcast.pure.pos
  (:require [ironcast.util :refer :all]
            [ironcast.pure.attr :refer :all]
            [clj-tuple :refer [tuple]]))

(defn bounds
  [world]
  (tuple (:width world) (:height world)))

(defn at
  [world pt]
  (-> world :by-pos (get pt)))

(defn ats
  [world pts]
  (mapcat #(at world %) pts))

(defn not-any-at?
  [world pt]
  (empty? (at world pt)))

(defn any-at?
  [world pt]
  (not-empty (at world pt)))

(defn adj-at
  [world pt]
  (mapcat #(at world %) (adj pt)))

(defn pred-at?
  [world pred pt]
  (boolean (some #(pred world %) (at world pt))))

(defn solid-at?
  [world pt]
  (pred-at? world solid? pt))

(defn not-solid-at?
  [world pt]
  (not (solid-at? world pt)))

(defn opaque-at?
  [world pt]
  (pred-at? world opaque? pt))

(defn transparent-at?
  [world pt]
  (not (opaque-at? world pt)))

(defn enemies-of-at
  [world ent pt]
  (filter #(enemy-of? world ent %) (at world pt)))

(defn enemy-of-at
  [world ent pt]
  (first (enemies-of-at world ent pt)))

(def enemy-of-at? (comp boolean enemy-of-at))

(defn enemies-of-in-pts
  [world ent pts]
  (mapcat #(enemies-of-at world ent %) pts))

(defn ally-of-at
  [world ent pt]
  (first (filter #(ally-of? world ent %) (at world pt))))

(def ally-of-at? (comp boolean ally-of-at))

(defn player-at
  [world pt]
  (first (filter #(player? world %) (at world pt))))

(def player-at? (comp boolean player-at))

(defn enemy-at
  [world pt]
  (first (filter #(enemy? world %) (at world pt))))

(def enemy-at? (comp boolean enemy-at))



(defn in
  [world rect]
  (ats world (rect-pts rect)))

(defn pos
  [world ent]
  (-> world :pos (get ent)))

(defn pos-adj?
  [world ent pt]
  (if-let [p (pos world ent)]
    (adj? p pt)
    false))

(defn adj-to
  [world ent]
  (adj-at world (pos world ent)))

(defn distance
  "Calculate the manhattan distance
   between entity `a` and `b`"
  [em entity-a entity-b]
  (let [a (pos em entity-a)
        b (pos em entity-b)]
    (or (and a b (manhattan-dist a b))
        Integer/MAX_VALUE)))

(defn unput
  [world ent]
  (if-let [p (pos world ent)]
    (-> (dissoc-in world [:pos ent])
        (update-in [:by-pos p] disj ent))
    world))

(defn- just-put
  [world ent pt]
  (-> (unput world ent)
      (assoc-in [:pos ent] pt)
      (update-in [:by-pos pt] set-conj ent)))

(defn in-bounds?
  [world [x y]]
  (and
    (< -1 x (:width world 32))
    (< -1 y (:height world 32))))

(defn can-put?
  [world ent pt]
  (and (or (not-solid-at? world pt)
           (decor? world ent))
       (in-bounds? world pt)))

(defn try-put
  [world ent pt]
  (if (can-put? world ent pt)
    (success (just-put world ent pt))
    (fail world)))

(defn put
  [world ent pt]
  (first (try-put world ent pt)))

(defn try-put-adj
  [world ent pt]
  (if (adj? (pos world ent) pt)
    (try-put world ent pt)
    (fail world)))

(defn select-at
  [world pt]
  (reduce select world (at world pt)))

(defn selectable-at
  [world pt]
  (first (filter #(selectable? world %) (at world pt))))

(defn selectable-at?
  [world pt]
  (boolean (selectable-at world pt)))

(defn select-only-at
  [world pt]
  (if-let [e (selectable-at world pt)]
    (select-only world e)
    world))

(defn select-in
  [world rect]
  (reduce select world (in world rect)))

(defn door-at
  [world pt]
  (first (filter #(door? world %) (at world pt))))

(def door-at? (comp boolean door-at))

(defn open-at?
  [world pt]
  (pred-at? world open? pt))

(defn closed-at?
  [world pt]
  (pred-at? world closed? pt))


