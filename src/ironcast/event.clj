(ns ironcast.event
  (:require [clojure.core.async :refer [chan mult go go-loop <! >!] :as async]
            [ironcast.state :as state]
            [ironcast.util :refer :all]
            [ironcast.pure.act :as act]
            [ironcast.pure.pos :as pos]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.create :as create]
            [ironcast.db :as db]))

(defonce act-chan (chan))
(defonce act-mult (mult act-chan))

(defn put-act!
  [action]
  (async/put! act-chan action))

(defmulti act-applied (fn [world event] (:type event)))

(defonce console-act-logger
  (let [c (chan)]
    (async/tap act-mult c)
    (go-loop
      []
      (when-let [msg (act/console-log @state/world (<! c))]
        (println msg))
      (recur))))


(defn act-apply
  [event]
  (dosync
    (let [w @state/world
          [nw success?] (act/applicate w event)]
      (when success?
        (ref-set state/world nw)
        (when-let [l (act/log nw event)]
          (alter state/log #(apply conj % l)))
        (when-let [wt (act/world-text nw event)]
          (alter state/world-text concat wt))
        true))))

(defonce act-applier
  (let [c (chan)]
    (async/tap act-mult c)
    (go-loop
      []
      (try
        (let [a (<! c)]
          (when (act-apply a)
            (act-applied @state/world a)))
        (catch Exception e
               (.printStackTrace ^Exception e)))
      (recur))))

(defmethod act-applied :default [_ _] nil)

(defn attack-ent-shift
  [world event]
  (let [attacker (:ent event)
        defender (:target event)
        a (pos/pos world attacker)
        b (pos/pos world defender)]
    (when (and a b)
      (let [[x y] (mult-ptn (direction a b) 8)]
        (swap! state/ent-shift assoc attacker (tuple 6 x y))))))

(defmethod act-applied :attack
  [world event]
  (attack-ent-shift world event))

(defmethod act-applied :trip
  [world event]
  (attack-ent-shift world event))

(defmethod act-applied :transition
  [world event]
  (println "Transit to" event)
  (let [[new-world success?]
        (try-state [world nil]
                   (create/try-create-world
                     {:gid (:gid @state/world)}
                     @state/db
                     (db/find-map @state/db (:to event)))
                   (let [points (take 6 (pos/starting-pts world))]
                     (create/try-create-many
                       world
                       (fn [w [player pt]]
                         (let [ent (:ent player)]
                           (pos/try-put
                             (create/unsnapshot w player)
                             ent
                             pt)))
                       (map tuple (:players event) points))))]
    (if success?
      (dosync
        (ref-set state/world new-world)))))
