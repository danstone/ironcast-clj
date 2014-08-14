(ns ironcast.pure.move
  (:require [ironcast.pure
             [pos :refer :all]
             [attr :refer :all]
             [time :as time]
             [vis :as vis]]
            [ironcast.util :refer :all]
            [clj-tiny-astar.path :refer [a*]]
            [clj-tuple :refer [tuple]]))


(defn move
  [world ent pt]
  (add-attr world ent :move-goal pt))

(defn goal
  [world ent]
  (attr world ent :move-goal))

(defn path
  [world ent]
  (attr world ent :path))

(defn- just-add-path
  [world ent path]
  (add-attr world ent :path path))

(defn try-add-path
  [world ent path]
  (let [g (goal world ent)]
    (if (and (= (last path) g)
             (adj? (first path) (pos world ent)))
      (success (just-add-path world ent path))
      (fail world))))

(defn add-path
  [world ent path]
  (first (try-add-path world ent path)))

(defn add-paths
  [world ent-path-pairs]
  (reduce (fn [w [e p]] (add-path w e p)) world ent-path-pairs))

(defn rem-path
  [world ent]
  (rem-attr world ent :path))

(defn try-pop-path
  [world ent]
  (let [[_ & rest] (path world ent)]
    (if rest
      (try-add-path world ent rest)
      (success (rem-path world ent)))))

(defn- try-step-to-turns
  [world ent pt]
  (if-let [pos (pos world ent)]
    (if (<= (cost pos pt) (current-ap world ent))
      (try-state [world world]
         (try-put-adj world ent pt)
         (success (sub-ap world ent (cost pos pt))))
      (fail world))
    (fail world)))

(defn- try-step-to
  [world ent pt]
  (if (time/turns? world)
    (try-step-to-turns world ent pt)
    (try-put-adj world ent pt)))

(defn try-step
  [world ent]
  (let [[n] (path world ent)]
    (if n
      (try-state [world world]
                 (try-step-to world ent n)
                 (try-pop-path world ent)
                 (success (rem-flag world ent :stuck)))
      (fail world))))

(defn step
  [world ent]
  (first (try-step world ent)))

(defn make-stuck
  [world ent]
  (add-flag world ent :stuck))
(defn unstuck
  [world ent]
  (rem-flag world ent :stuck))
(defn stuck?
  [world ent]
  (has-flag? world ent :stuck))

(defn stucks
  [world]
  (-> world :with-flag :stuck))

(defn step-or-stuck
  [world ent]
  (let [[w success?] (try-step world ent)]
    (if success?
      (unstuck w ent)
      (make-stuck world ent))))

(defn step-all
  [world]
  (let [all (-> world :with-attr :path)]
    (reduce step-or-stuck world all)))

(defn walkable?
  [world pt]
  (not-solid-at? world pt))

(defn player-walkable?
  [world pt]
  (and (walkable? world pt)
       (vis/explored? world pt)))

(defn find-path
  [world from to]
  (when (walkable? world to)
    (rest (a* (bounds world) #(or (walkable? world %)
                                  (= from %)) from to))))

(defn find-path-to-adjacent
  "Find a path to a cell adj to `b` from `a`"
  [world a b]
  (let [adj (sort-by #(manhattan-dist a %) (adj b))]
    (loop [adj adj]
      (if-let [p (first adj)]
        (if-let [path (and (not-solid? world p) (seq (find-path world not-solid? a p)))]
          path
          (recur (rest adj)))))))

(defn find-player-path
  [world from to]
  (when (player-walkable? world to)
    (rest (a* (bounds world) #(or (player-walkable? world %)
                                  (= from %)) from to))))

(defn find-path-for
  [world ent to]
  (if (player? world ent)
    (find-player-path world (pos world ent) to)
    (find-path world (pos world ent) to)))


(defn make-path
  [world ent]
  (let [b (goal world ent)
        a (pos world ent)]
    (and a b
         (find-path-for world ent b))))

(defn should-path?
  [world ent]
  (let [p (path world ent)
        pos (pos world ent)
        g (goal world ent)]
    (or (and (not p)
             (not= pos g))
        (and (or (not= (last p) g)
                 (has-flag? world ent :stuck))
             (not= pos g)))))

(defn should-paths
  [world]
  (let [all (-> world :with-attr :move-goal)]
    (filter #(should-path? world %) all)))

(defn should-re-goal?
  [world ent]
  (and (stuck? world ent)
       (adj? (goal world ent) (pos world ent))
       (solid-at? world (goal world ent))))

(defn needing-re-goal
  [world]
  (filter #(should-re-goal? world %) (stucks world)))

(defn find-nearest-goal
  [world ent]
  (first (take 10 (flood (goal world ent) #(solid-at? world %)))))

(defn try-re-goal
  [world ent]
  (if-let [g (find-nearest-goal world ent)]
    (success (move world ent g))
    (fail world)))

(defn re-goal
  [world ent]
  (first (try-re-goal world ent)))

(defn re-goal-all
  [world]
  (reduce re-goal world (needing-re-goal world)))