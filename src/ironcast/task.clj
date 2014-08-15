(ns ironcast.task
  (:require
    [ironcast.util :refer :all]
    [clojure.core.async :refer [go go-loop chan timeout <! >! alt!] :as async]
    [ironcast.api :as api]
    [ironcast.pure.move :as move]
    [ironcast.pure.vis :as vis]
    [ironcast.pure.attr :as attr]
    [ironcast.pure.ai :as ai]
    [ironcast.state :as state]
    [ironcast.event :as event]
    [clj-tuple :refer [tuple]]
    [ironcast.ui :as ui]
    [clojure.set :as set]
    [ironcast.pure.time :as time]))



(defmacro looper
  [every & body]
  `(let [cancel# (chan)]
     (reify Comp
       (start [_#]
         (go-loop
           []
           (alt! cancel# :done
                 (timeout ~every) ([_#] ~@body (recur)))))
       (stop [_#]
         (async/put! cancel# :done)))))



(defn do-paths
  []
  (let [w @state/world
        paths (for [e (move/should-paths w)
                    :let [p (move/make-path w e)]
                    :when (seq p)]
                [e p])]
    (api/update-world move/add-paths paths)))

(defonce pather
  (looper
    (api/setting :path-ms 500)
    (do-paths)))

(defn do-ui-paths
  []
  (when (and @api/turns?
             (ui/should-path? @state/ui))
    (let [path (ui/find-selected-path @state/ui @state/world)]
      (swap! state/ui assoc :path path))))

(defonce ui-pather
  (looper
    (api/setting :ui-path-ms 250)
    (do-ui-paths)))

(defonce re-move-goals
  (looper
    (api/setting :regoal-ms 500)
    (api/update-world move/re-goal-all)))

(defonce mover
  (looper
    (api/setting :move-ms 150)
    (api/update-world move/step-all)))

(defonce looker
  (looper
    (api/setting :vis-ms 250)
    (let [vis (vis/player-visibility @state/world)]
      (api/update-world vis/add-visibility vis))))

(defn do-ai
  [ent]
  (when-not @api/player?
    (let [world @state/world
          observed (ai/observe world ent)]
      (swap! state/ai-state assoc ent observed)
      (when-let [act (ai/decide world ent observed)]
        (println "Action: " act)
        (api/do-act act))
      (when (and (not (ai/done? world ent))
                 (not (time/player? world)))
        (api/update-world ai/done ent)))))

(defn ai-loop
  [ent]
  (looper
    (api/setting :ai-ms 1000)
    (try
      (do-ai ent)
      (catch Throwable t
        (.printStackTrace t)
        (api/update-world attr/rem-flag ent :ai)))))

(defn ai-adds
  []
  (let [world @state/world
        ai @state/ai-loops]
    (doseq [e (attr/ai world)
            :when (not (get ai e))]
      (swap! state/ai-loops
             assoc e (let [l (ai-loop e)]
                       (println "Starting ai-loop" e)
                       (start l)
                       l)))))

(defn ai-rems
  []
  (let [world @state/world]
    (doseq [[e loop] @state/ai-loops
            :when (not (attr/ai? world e))]
      (println "Stopping ai-loop" e)
      (stop loop)
      (swap! state/ai-loops dissoc e))))

(defn ai-done
  []
  (let [world @state/world]
    (when (and (ai/all-done? world)
               (time/ai? world))
      (api/flip-turn))))

(defn maintain-ai
  []
  (ai-rems)
  (ai-adds)
  (ai-done))

(defonce ai-maintainer
  (looper
    ;every
    (api/setting :ai-ms 1000)
    ;;do
    (maintain-ai)))

(defn tick-ent-shift
  [ent-shift]
  (->> (for [[id [t x y]] ent-shift
             :when (pos? t)]
         (tuple id (tuple (dec t) x y)))
       (into {})))

(defn tick-animations!
  []
  (swap! state/ent-shift tick-ent-shift))

(defonce animator
  (looper
    (api/setting :anim-ms 15)
    (tick-animations!)))

(defn stop-all
  []
  (stop pather)
  (stop ui-pather)
  (stop mover)
  (stop re-move-goals)
  (stop looker)
  (stop animator)
  (stop ai-maintainer))

(defn start-all
  []
  (start pather)
  (start ui-pather)
  (start mover)
  (start re-move-goals)
  (start looker)
  (start animator)
  (start ai-maintainer))