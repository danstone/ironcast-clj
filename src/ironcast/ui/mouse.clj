(ns ironcast.ui.mouse
  (:require [ironcast.api :as api]
            [ironcast.internals.gfx :as gfx]))


(defn button-sprite
  [button]
  (case button
    :left (api/sprite :mouse-left)
    :right (api/sprite :mouse-right)))

(defn draw-button!
  [button x y]
  (gfx/draw-sprite! (button-sprite button) x y))

(defn select-button
  []
  (let [commands (api/setting :commands)
        [_ select] (:select commands)]
    select))

(defn draw-select-mouse!
  [x y]
  (gfx/draw-sprite! (api/sprite :mouse-select) x y)
  (draw-button! (select-button) x y))


(defn draw-grey-casting-mouse!
  [x y]
  (gfx/draw-sprite! (api/sprite :mouse-magic-grey) x y)
  (when-let [spr (api/sprite (:sprite @api/casting))]
    (gfx/draw-sprite! spr (+ x 24) (- y 24))))

(defn draw-casting-mouse!
  [x y]
  (gfx/draw-sprite! (api/sprite :mouse-magic) x y)
  (when-let [spr (api/sprite (:sprite @api/casting))]
    (gfx/draw-sprite! spr (+ x 24) (- y 24))))

