(ns ironcast.screens.stats
  (:require [ironcast.ui.shell :as shell]
            [ironcast.ui.debug :as debug]
            [ironcast.ui.controls :as controls]
            [ironcast.ui.players :as players]
            [ironcast.ui.mouse :as mouse]
            [ironcast.ui.info :as info]
            [ironcast.ui.base :refer :all]
            [ironcast.api :as api]
            [ironcast.util :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.pure.act :as act]
            [ironcast.state :as state]
            [ironcast.gfx.world :as wgfx]))

(defn in-player-mouse
  [x y]
  (cond
    :otherwise
    (gfx/draw-sprite! (api/sprite :mouse) x y)))

(defn draw-mouse
  []
  (let [[x y] @api/screen-pos
        y (- y 32)
        in-game? @api/mouse-in-game?
        in-player? @api/mouse-in-player]
    (cond
      in-player? (in-player-mouse x y)
      :otherwise (gfx/draw-sprite! (api/sprite :mouse) x y))))

(defn draw-player
  [player x y]
  (let [x (+ x 32)
        y (- y 32)]
    (draw-text! (api/attr player :name "???") x y)
    (wgfx/draw-creature @state/world
                        (first @api/selected)
                        x
                        (- y 96)
                        64
                        64)))

(defn draw
  []
  (gfx/with-font-color @api/default-font :off-white
    (when-not @api/info
      (shell/draw-shell)
      (players/draw-players)
      (controls/draw-menu-buttons)
      (let [[x y w h] @api/game-rect
            world @state/world
            player (first @api/selected)]
        (draw-player player x y)
        (draw-stats world player (+ x 128) (- y 32))))
    (info/draw-info-pane)
    (draw-mouse)))
