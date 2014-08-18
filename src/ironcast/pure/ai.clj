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


(defprotocol Behaviour
  (act [this world ent context]))

(defrecord Sequence [behaviours]
  Behaviour
  (act [this world ent context]
    (let [f (fn [st b]
              (if (= (:status st) :success)
                (let [result (act b world ent (:context st))]
                  (case (:status result)
                    :success (-> st
                                 (update :context merge (:context result))
                                 (update :actions concat (:actions result)))
                    (->
                      st
                      (update :context merge (:context result))
                      (assoc :status :failed))))
                st))]
      (reduce f {:status :success
                 :context context} (:behaviours this)))))

(defrecord Selector [behaviours]
  Behaviour
  (act [this world ent context]
    (let [f (fn [st b]
              (if (not= (:status st) :success)
                (let [result (act b world ent (:context st))]
                  (case (:status result)
                    :success (-> st
                                 (update :context merge (:context result))
                                 (update :actions concat (:actions result))
                                 (assoc :status :success))
                    (-> st
                        (update :context merge (:context result)))))
                st))]
      (reduce f {:status :failed
                 :context context} (:behaviours this)))))

(defmacro finder
  [kw context form]
  `(if (~kw ~context)
     {:status :success}
     (let [found# ~form]
       (when found#
         {:status :success
          :context {~kw found#}}))))

(def bfind-adj
  (reify Behaviour
    (act [this world ent context]
      (finder :adj context
              (seq (adj-to world ent))))))

(def bfind-adj-enemies*
  (reify Behaviour
    (act [this world ent context]
      (finder :adj-enemies context
              (seq (filter #(enemy-of? world ent %) (:adj context)))))))

(def bfind-adj-enemy*
  (reify Behaviour
    (act [this world ent context]
      (finder :adj-enemy context
              (first (:adj-enemies context))))))

(def battack-adj*
  (reify Behaviour
    (act [this world ent context]
      (let [enemy (:adj-enemy context)
            pt (pos world enemy)]
        (when (and enemy pt (act/can? world ent pt act/attack-action))
          {:status :success
           :actions [(act/prepare world ent pt act/attack-action)]})))))

(def bfind-enemies
  (reify Behaviour
    (act [this world ent context]
      (finder :enemies context
              (seq (visible-by-sorted world ent (enemies-of world ent)))))))

(def bfind-attack-targets*
  (reify Behaviour
    (act [this world ent context]
      (finder :attack-targets context
              (seq (sort-by #(attack-priority world ent %) (:enemies context)))))))

(def bfind-pathable-attack-targets*
  (reify Behaviour
    (act [this world ent context]
      (finder :pathable-attack-targets context
              (seq (pathable-adj-targets world ent (:attack-targets context)))))))

(def bfind-pathable-attack-target*
  (reify Behaviour
    (act [this world ent context]
      (finder :pathable-attack-target context
              (first (:pathable-attack-targets context))))))

(def battack*
  (reify Behaviour
    (act [this world ent context]
      (let [[_ path] (:pathable-attack-target context)
            pt (last path)]
        (when (and pt
                 path
                 (act/can? world ent pt act/move-action)
                 (<= (cost (pos world ent) (first path)) (current-ap world ent)))
          {:status :success
           :actions [(act/prepare world ent pt act/move-action)]})))))

(def bspent
  (reify Behaviour
    (act [this world ent context]
      (when (= 0 (current-ap world ent))
        {:status :success
         :actions [(act/prepare world ent nil done-action)]}))))

(def bdone
  (reify Behaviour
    (act [this world ent context]
      {:status :success
       :actions [(act/prepare world ent nil done-action)]})))

(def bfind-attack-targets
  [bfind-enemies bfind-attack-targets*])

(def bfind-pathable-attack-targets
  [bfind-attack-targets bfind-pathable-attack-targets*])

(def bfind-pathable-attack-target
  [bfind-pathable-attack-targets bfind-pathable-attack-target*])

(def bfind-adj-enemies
  [bfind-adj bfind-adj-enemies*])

(def bfind-adj-enemy
  [bfind-adj-enemies bfind-adj-enemy*])

(def battack-adj
  [bfind-adj-enemy battack-adj*])

(def battack
  [bfind-pathable-attack-target battack*])

(def bstay-adj
  [bfind-adj-enemy bdone])

(def basic
  [:select
       bspent
       battack-adj
       bstay-adj
       battack
       bdone])

(defn compile-tree
  [behaviour]
  (cond
    (and (vector? behaviour) (= (first behaviour) :select))
    (->Selector (map compile-tree (drop 1 behaviour)))
    (vector? behaviour)
    (->Sequence (map compile-tree behaviour))
    :else behaviour))

(def basic-compiled
  (compile-tree basic))

(defn mdone
  [world ent observed]
  (act/prepare world ent nil done-action))

(defn mspent
  [world ent observed]
  (when (= 0 (current-ap world ent))
    (mdone world ent observed)))

(defn decide
  [world ent]
  (:actions (act basic-compiled world ent {})))

