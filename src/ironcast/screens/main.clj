(ns ironcast.screens.main
  (:require [ironcast.ui.shell :as shell]
            [ironcast.ui.debug :as debug]
            [ironcast.ui.controls :as controls]
            [ironcast.ui.players :as players]
            [ironcast.ui.mouse :as mouse]
            [ironcast.ui.actions :as actions]
            [ironcast.api :as api]
            [ironcast.util :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.pure.act :as act]
            [ironcast.state :as state]))

(def mouse-sprite
  (>> (let [selected (first @api/selected)
            pt @api/world-cell
            world @state/world]
        (->> (for [action act/default-actions]
               (cond
                 (act/can? world selected pt action)
                   (:mouse-sprite action)
                 (act/applies? world selected pt action)
                   (:mouse-sprite-grey action)))
             (some identity)))))

(defn in-game-mouse
  [x y]
  (cond
    @api/can-cast-at-mouse? (mouse/draw-casting-mouse! x y)
    @api/casting (mouse/draw-grey-casting-mouse! x y)
    @api/player-at-mouse (mouse/draw-select-mouse! x y)
    :otherwise (gfx/draw-sprite! (api/sprite (or @mouse-sprite :mouse)) x y)))

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
      in-game? (in-game-mouse x y)
      in-player? (in-player-mouse x y)
      :otherwise (gfx/draw-sprite! (api/sprite :mouse) x y))))

(defn draw
  []
  (gfx/with-font-color @api/default-font :off-white
    (shell/draw-shell)
    (players/draw-players)
    (controls/draw-turn-timer)
    (controls/draw-descr-bar)
    (controls/draw-menu-buttons)
    (controls/draw-action-buttons)
    (controls/draw-action-bar)
    (actions/draw)
    (debug/draw-debug)
    (draw-mouse)))
