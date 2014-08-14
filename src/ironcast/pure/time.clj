(ns ironcast.pure.time
  (:require [ironcast.util :refer :all]
            [ironcast.pure
             [attr :refer [clear-attr] :as attr]]))

(defn stop-motion
  "Remove all known paths and move-goals effectively
   stopping the world in motion"
  [world]
  (-> (clear-attr world :path)
      (clear-attr :move-goal)))

(defn clear-done
  [world]
  (dissoc world :done))

(defn into-turns
  [world mode]
  (-> world
      stop-motion
      clear-done
      (assoc :mode mode)))

(defn real
  [world]
  (assoc world :mode :real))

(defn real?
  [world]
  (= (:mode world :real) :real))

(defn player
  [world]
  (-> (into-turns world :player)
      attr/refresh-player-aps))

(defn player?
  [world]
  (= (:mode world) :player))

(defn enemy
  [world]
  (-> (into-turns world :enemy)
      attr/unselect-all
      attr/refresh-enemy-aps))

(defn enemy?
  [world]
  (= (:mode world) :enemy))

(def turns? (comp boolean
                  #{:player
                    :enemy}
                  :mode))

(defn ai?
  [world]
  (enemy? world))

(defn flip
  [world]
  (let [f (case (:mode world)
            :player enemy
            player)]
    (f world)))

(defn combat
  [world]
  (player world))

(defn combat?
  [world]
  (turns? world))

(defn peace
  [world]
  (real world))

(defn flip-mode
  [world]
  (if (turns? world)
    (peace world)
    (combat world)))



