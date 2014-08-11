(ns ironcast.ui.shell
  (:require [ironcast.internals.gfx :as gfx]
            [ironcast.util :refer :all]
            [ironcast.ui.base :refer :all]
            [ironcast.api :as api]
            [clj-tuple :refer [tuple]]))


(defn black-out
  []
  (let [[x y] @api/top-left
        sw @api/screen-width
        rem (mod sw 32)]
    (draw-blank! x
                 (- y)
                 128
                 @api/screen-height)
    (draw-blank! (- (/ sw 2) 128)
                 (- y)
                 128
                 @api/screen-height)
    (draw-blank! x
                 (- y)
                 sw
                 192)))

(defn backing
  []
  (let [[x y] @api/top-left
        sh @api/screen-height
        sw @api/screen-width
        [gx gy gw gh] @api/game-rect
        hrem (mod sh 32)
        h (int (Math/ceil (/ sh 32)))
        w (int (Math/ceil (/ sw 32)))]
    ;left
    (draw-blocks! (+ 128 x) y 1 h)
    (draw-blocks! x (- y 160) 4 1)
    (draw-blocks! x (- y 352) 4 1)
    (draw-blocks! x (- y 544) 4 1)
    ;right
    (let [x (- (/ sw 2) 160)
          x (+ x 32)]
      (draw-blocks! (- x 32) y 1 h)
      (draw-blocks! x (- y 160) 4 1)
      (draw-blocks! x (- y 352) 4 1)
      (draw-blocks! x (- y 544) 4 1)
      ;bottom-right
      (draw-blocks! x (+ (/ sh -2) hrem 128) 4 1)
      (draw-blocks! x (+ (/ sh -2) hrem 32) 4 1))
    ;mid
    (draw-blocks! x (+ (/ sh -2) hrem 224) w 1)
    (draw-blocks! (+ gx gw (* -12 32))
                  (+ (/ sh -2) hrem 224)
                  1
                  (/ (- (/ sh 2) hrem 160) 32))))

(defn draw-shell
  []
  (gfx/with-color
    :black
    (black-out))
  (backing))