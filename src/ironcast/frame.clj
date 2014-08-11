(ns ironcast.frame
  (:import (java.util.concurrent ConcurrentLinkedQueue))
  (:require [ironcast.state :as state]
            [ironcast.render :refer [render]]
            [ironcast.internals.input :as input]
            [ironcast.input :refer [handle-all]]
            [ironcast.ui :as ui]
            [ironcast.util :refer :all]
            [ironcast.pure.attr :as attr]
            [ironcast.pure.time :as time]
            [ironcast.api :as api]
            [clj-tuple :refer [tuple]]
            [ironcast.event :as event]))

(defn force-selection
  [world]
  (let [selected (attr/selected world)]
    (if (and (time/turns? world)
             (rest selected))
      (attr/select-only world (first selected))
      world)))


(defn missile-done?
  [missile]
  (=
    (div-ptn (:from missile) 32)
    (div-ptn (:to missile) 32)))

(defn move-missile
  [delta missile]
  (update missile
          :from #(add-pt % (mult-ptn
                             (direction % (:to missile))
                             (* delta 300)))))

(defn tick-missiles
  [world delta]
  (update world :missiles #(map (partial move-missile delta) %)))

(defn filter-missiles
  [world]
  (let [missiles (:missiles world)]
    (assoc world :missiles (filter (complement missile-done?) missiles))))

(defn fire-missiles!
  []
  (doseq [m (:missiles @state/world)
          :when (missile-done? m)]
    (event/put-act! m)))

(defn- do-on-main-actions
  []
  (loop [action (.poll ^ConcurrentLinkedQueue state/on-main-thread)]
    (when action
      (action)
      (recur (.poll ^ConcurrentLinkedQueue state/on-main-thread)))))

(def updater (agent nil))
(def gdx-input (input/->GdxInput))

(defn frame-background
  []
  (try
    (swap! state/input input/cycle-state (input/get-state gdx-input) gdx-input)
    (reset! state/commands
            (-> @state/input
                (input/commands-hit (:commands @state/settings))
                set))
    (handle-all @state/commands)
    (swap! state/ui ui/update-ui @state/input @state/cam @state/world)
    (let [delta @api/delta]
      (api/update-world #(-> %
                             force-selection
                             (tick-missiles delta))))
    (fire-missiles!)
    (api/update-world filter-missiles)
    (catch Exception e)))

(defn frame
  []
  (send-off updater (fn [_] (frame-background)))
  (input/hide-hw-cursor)
  (do-on-main-actions)
  (render)
  (await updater))