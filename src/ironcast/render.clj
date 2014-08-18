(ns ironcast.render
  (:import (java.util.concurrent ConcurrentLinkedQueue))
  (:require [ironcast.state :as state]
            [ironcast.internals.gfx :as gfx]
            [ironcast.gfx.world :refer [draw-world]]
            [ironcast.api :as api]
            [ironcast.ui.base :as base]
            [ironcast.screens.main :as main]))


(defn draw-game
  []
  (let [cam @state/cam
        world @state/world]
    (gfx/set-cam! cam)
    (draw-world world)
    (base/draw-path!)
    (base/draw-aoe!)
    (base/draw-los!)
    (base/draw-lasso!)
    (base/draw-world-text!)
    (gfx/release-cam! cam)))


(defn draw-fps
  []
  (let [[x y] @api/game-rect]
    (gfx/draw-text! @api/default-font
                    (str "fps " @api/fps)
                    x y)))

(defn render
  []
  (gfx/clear!)
  (when-let [b @state/batch]
    (gfx/with-batch
      b
      (draw-game)
      (main/draw))))