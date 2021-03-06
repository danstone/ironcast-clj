(ns ironcast.ui.players
  (:require [ironcast.internals.gfx :as gfx]
            [ironcast.gfx.world :as wgfx]
            [ironcast.util :refer :all]
            [ironcast.ui.base :refer :all :exclude [draw-stats]]
            [ironcast.api :as api]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.attr :as attr]
            [ironcast.state :as state]))

(defn draw-backing
  [player x y w h]
  (if (api/selected? player)
    (gfx/draw-backed-box! @api/blank x y w h :green)
    (draw-hover-border! x y w h)))

(defn draw-stats
  [player x y]
  (let [world @state/world]
    (let [[hp max-hp] (attr/hp world player)
          [ap max-ap] (attr/ap world player)
          text (str
                 "hp: " hp "/" max-hp "\n"
                 "ap: " ap "/" max-ap " \n"
                 "fatigue: " (attr/fatigue world player))]
      (draw-text! text (+ x 4) (- y 96)))))

(defn draw-player
  [x y player]
  (when player
    (let [w 128 h 160]
      (draw-backing player x y w h))
    (let [sprite (api/attr player :sprite)
          name (str (api/attr player :name "???") " " player)]
      (when (and sprite name)
        (gfx/draw-text! @api/default-font name
                        (+ x 3)
                        (- y 3))
        (wgfx/draw-creature @state/world player (+ x 16) (- y 96) 64 64)
        (draw-stats player x y)))))

(defn draw-players
  []
  (let [players @api/sorted-players]
    (dotimes [n 6]
      (let [[x y w h] (api/player-rect n)]
        (draw-player x (dec y) (nth players n nil))))))