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

(def done-action
  {:type :done})

(defmethod act/could? :done
  [world ent _ _]
  true)

(defmethod act/prepare :done
  [_ ent _ action]
  (assoc action :ent ent))

(defmethod act/try-perform :done
  [world {:keys [ent]}]
  (success (done world ent)))

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

(defn pathable-adj-targets
  [world ent targets]
  (lazy-seq
    (when-let [target (first targets)]
      (if-let [path (find-path-to-adj-ent world ent target)]
        (cons (tuple target (last path)) (pathable-adj-targets world ent (rest targets)))
        (pathable-adj-targets world ent (rest targets))))))

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
  (if (and world ent)
    (observe* world ent)))

(defn mattack-adj
  [world ent observed]
  (when-let [adj-enemies (:adj-enemies observed)]
    (when-let [target (first adj-enemies)]
      (act/prepare world ent (pos world target) act/attack-action))))

(defn mattack
  [world ent observed]
  (when-let [pathable (:pathable-attack observed)]
    (let [[_ pt] (first pathable)]
      (act/prepare world ent pt act/move-action))))

(defn mdone
  [world ent observed]
  (act/prepare world ent nil done-action))

(defn decide
  [world ent observed]
  (-> (for [f [mattack-adj mattack mdone]
             :let [action (f world ent observed)]
             :when action]
         action)
       first))

