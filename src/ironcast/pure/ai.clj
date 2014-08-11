(ns ironcast.pure.ai
  (:require [ironcast.util :refer :all]
            [ironcast.pure
             [attr :refer :all]]
            [clojure.set :as set]))

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

(defn clear-done
  [world]
  (dissoc world :done))