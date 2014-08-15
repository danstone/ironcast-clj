(ns ironcast.pure.ai
  (:require [ironcast.util :refer :all]
            [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]
             [move :refer :all]
             [vis :refer :all]]
            [clojure.set :as set]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.time :as time]
            [ironcast.pure.act :as act]))

(defn done
  [world ent]
  (update world :done set-conj ent))

(defn done?
  [world ent]
  (-> world :done (contains? ent)))

(defn all-done?
  [world]
  (let [all (ai world)]
    (set/superset? (:done world) all)))


(defn look
  [world ent]
  (let [enemies  (visible-by-sorted world ent (enemies-of world ent))
        allies  (visible-by-sorted world ent (allies-of world ent))]
    {:ent ent
     :enemies enemies
     :allies allies
     :nearest-ally (first allies)
     :nearest-enemy (first enemies)}))

(defn attack-priority
  [world ent target]
  (let [dist (manhattan-dist
               (pos world ent)
               (pos world target))]
    (/ 1 dist)))



(defn pathable-adj-targets
  [world ent targets]
  (lazy-seq
    (when-let [target (first targets)]
      (if-let [path (find-path-to-adj-ent world ent target)]
        (cons (tuple target path) (pathable-adj-targets world ent (rest targets)))
        (pathable-adj-targets world ent (rest targets))))))


(defn find-adj
  [state world ent]
  (let [all (adj-to world ent)]
    (assoc state
      :adj-enemies (sort-by #(attack-priority world ent %)
                            (filter #(enemy-of? world ent %) all))
      :adj-allies (filter #(ally-of? world ent %) all))))


(defn find-attack-targets
  [state world ent]
  (let [enemies (:enemies state)]
    (assoc state :attack (sort-by #(attack-priority world ent %) enemies))))

(defn find-pathable-attack-targets
  [state world ent]
  (assoc state
    :pathable-attack (pathable-adj-targets world ent (:attack state))))

(defn observe*
  [world ent]
  (-> (look world ent)
      (find-adj world ent)
      (find-attack-targets world ent)
      (find-pathable-attack-targets world ent)))

(defn observe
  [world ent]
  (if (and ent
           (or (time/real? world)
               (not (time/enemy? world))))
    (observe* world ent)))



