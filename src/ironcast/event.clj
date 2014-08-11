(ns ironcast.event
  (:require [clojure.core.async :refer [chan mult go go-loop <! >!] :as async]
            [ironcast.state :as state]
            [ironcast.util :refer :all]
            [ironcast.pure.act :as act]
            [ironcast.pure.pos :as pos]
            [clj-tuple :refer [tuple]]))

(defonce act-chan (chan))
(defonce act-mult (mult act-chan))

(defn put-act!
  [action]
  (async/put! act-chan action))

(defmulti act-world-text (fn [world event] (:type event)))
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
        (alter state/world-text concat (act-world-text nw event))))))

(defonce act-applier
  (let [c (chan)]
    (async/tap act-mult c)
    (go-loop
      []
      (try
        (let [a (<! c)]
          (act-apply a)
          (act-applied @state/world a))
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
