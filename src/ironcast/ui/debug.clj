(ns ironcast.ui.debug
  (:require [ironcast.internals.gfx :as gfx]
            [ironcast.util :refer :all]
            [ironcast.ui.base :refer :all]
            [ironcast.api :as api]
            [ironcast.state :as state]
            [clojure.string :as str]))


(defn draw-fps
  [x y]
  (draw-text! (str "fps: " @api/fps) x y))


(defn draw-mouse-positions
  [x y]
  (draw-text! (str "world-cell: " @api/world-cell "\n"
                   "mouse-pos: " @api/mouse-pos "\n"
                   "cam-pos: " @api/cam-pos "\n"
                   "screen-pos: " @api/screen-pos " in game: " @api/mouse-in-game?) x y))

(defn draw-lasso-state
  [x y]
  (draw-text!
    (str "lasso: " @api/lasso) x y))

(defn draw-map
  [st x y]
  (draw-text!
    (str/join "\n" st)
    x y))

(defn draw-mode
  [x y]
  (draw-text! (str "mode: " (name @api/mode)) x y))

(defn draw-debug
  []
  (when @api/debug?
    (let [[x y] @api/game-rect]
      (draw-fps x y)
      (draw-mouse-positions x (- y 18))
      (draw-mode x (- y 90)))))