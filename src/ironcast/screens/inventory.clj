(ns ironcast.screens.inventory
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
            [ironcast.gfx.world :as wgfx]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.attr :as attr]
            [ironcast.pure.pos :as pos]
            [ironcast.ui.actions :as actions]))

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
        y (- y 32 6)]
    (draw-text! (api/attr player :name "???") x y)
    (wgfx/draw-creature @state/world
                        (first @api/selected)
                        x
                        (- y 96)
                        64
                        64)))

(defn draw-shell
  [x y w h]
  (draw-blocks! x y 5 1)
  (draw-blocks! (+ x 128) y 1 5)
  (draw-blocks! x (- y 160) (/ w 32) 1)
  (draw-blocks! (+ x 320) (- y 192) 1 (- (/ h 32) 5)))

(defn draw-labels
  [x y]
  (let [x (+ x 6)
        y (- y 192 10)]
    (draw-text! "head: " x y)
    (draw-text! "torso: " x (- y 32))
    (draw-text! "boots: " x (- y 64))
    (let [x (+ x 96 6)]
      (draw-text! "gloves: " x y)
      (draw-text! "cloak: " x (- y 32))
      (draw-text! "misc: " x (- y 64)))
    (draw-text! "hands: " x (- y 128))
    (draw-text! "gear: " x (- y 192 14))
    (draw-text! "floor: " x (- y 288 14))))

(def slots
  {:head [(tuple 0 0)]
   :torso [(tuple 0 32)]
   :boot [(tuple 0 64)]
   :hand [(tuple 0 128) (tuple 32 128)]
   :gear (for [x (range 6)
               y (range 2)]
           (tuple (* 32 x) (+ 192 (* 32 y))))
   :floor (for [x (range 6)
                y (range 2)]
            (tuple (* 32 x) (+ 288 (* 32 y))))
   :glove [(tuple 96 0)]
   :cloak [(tuple 96 32)]
   :misc [(tuple 96 64) (tuple 128 64) (tuple 160 64)]})

(defn draw-slots
  [x y]
  (let [x (+ x 64)
        y (- y 224)]
    (doseq [[x2 y2] (apply concat (vals slots))]
      (draw-slot! (+ x x2)
                  (- y y2)))))

(defn draw-item-hover
  [e x y]
  (when (api/mouse-in? x (+ y 32) 32 32)
    (swap! state/ui assoc :act-target [:item e])))

(defn draw-item
  [world e x y]
  (gfx/draw-sprite! (attr/attr world e :sprite)
                    x
                    y)
  (let [q (attr/quantity world e)]
    (when (> q 1)
      (draw-text! (str q) (+ x 16) (+ y 12))))
  (draw-item-hover e x y))

(defn draw-equipment
  [world player x y]
  (draw-labels x y)
  (draw-slots x y)
  (let [equip (attr/equipped world player)]
    (doseq [e equip
            :let [in (attr/attr world e :in)
                  slot (attr/attr world e :slot)
                  slot-pos (nth (get slots slot) in nil)]
            :when slot-pos
            :let [[x2 y2] slot-pos
                  x (+ x x2 64)
                  y (- y y2 224)]]
     (draw-item world e x y))))

(defn draw-on-floor
  [world player x y]
  (let [pt (pos/pos world player)
        items (pos/items-at world pt)]
   (doseq [[e [x2 y2]] (map tuple items (:floor slots))
           :when (and x2 y2)
           :let [x (+ x x2 64)
                 y (- y y2 224)]]
     (draw-item world e x y))))

(defn draw-bag
  [world x y]
  (loop [items (:bag world)
         x (+ x 352)
         y (- y 224)]
    (let [item (first items)]
      (when item
        (draw-item world item x y)
        (recur (rest items) (+ x 32) y)))))

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
        (draw-shell x y w h)
        (draw-equipment world player x y)
        (draw-on-floor world player x y)
        (draw-bag world x y)
        (draw-player player x y)
        (draw-stats world player (+ x 160) (- y 0))
        (debug/draw-fps x y)))
    (controls/draw-descr-bar)
    (controls/draw-action-buttons)
    (controls/draw-action-bar)
    (actions/draw)
    (info/draw-info-pane)
    (draw-mouse)))
