(ns ironcast.ui.info
  (:require [ironcast.ui.base :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.api :as api]
            [ironcast.state :as state]
            [ironcast.pure.attr :as attr]
            [ironcast.gfx.world :as wgfx]))

(defmulti draw-info (fn [[type _] x y w h] type))

(defmethod draw-info :default
 [_ x y w h]
  (draw-text! "???" (+ x 32) (- y 32)))

(defn draw-stats
  [world ent x y]
  (gfx/with-color :light-yellow
    (gfx/draw-border! @api/blank x y 256 96 1))
  (let [x (+ x 6)
        y (- y 6)]
      (draw-hp world ent x y)
      (draw-ap world ent x (- y 16))
      (draw-fatigue world ent x (- y 32))))

(defmethod draw-info :ent
  [[_ ent] x y w h]
  (let [world @state/world
        attrs (attr/all world ent)]
    (when ent
      (draw-text! (:name attrs "???") (+ x 32) (- y 32))
      (wgfx/draw-creature world ent (+ x 32) (- y 128) 64 64)
      (draw-text-wrapped! (:descr attrs "???") (+ x 32)
                          (- y 160) 256)
      (draw-stats world ent (+ x 128) (- y 32)))))

(defn draw-info-pane
  []
  (when-let [info @api/info]
    (let [w 768
          h 512
          [x y] [(/ w -2) (/ h 2)]]
      (gfx/draw-box! @api/blank x y w h :transparent-black :yellow)
      (draw-info info x y w h))))

