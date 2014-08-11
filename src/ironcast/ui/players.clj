(ns ironcast.ui.players
  (:require [ironcast.internals.gfx :as gfx]
            [ironcast.util :refer :all]
            [ironcast.ui.base :refer :all]
            [ironcast.api :as api]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.attr :as attr]
            [ironcast.state :as state]))

(defn draw-backing
  [x y w h player]
  (if (api/selected? player)
    (gfx/draw-backed-box! @api/blank x y w h :green)
    (draw-hover-border! x y w h)))



(defn draw-player
  [x y player]
  (when player
    (let [w 128 h 160]
      (draw-backing x y w h player))
    (let [sprite (api/attr player :sprite)
          name (str (api/attr player :name "???") " " player)]
      (when (and sprite name)
        (gfx/draw-text! @api/default-font name
                        (+ x 3)
                        (- y 3))
        (gfx/draw-sprite! sprite (+ x 16) (- y 96) 64 64)
        (let [[hp max-hp] (attr/hp @state/world player)
              [ap max-ap] (attr/ap @state/world player)]
          (gfx/draw-text! @api/default-font (str
                                              hp "/" max-hp " hp\n"
                                              ap "/" max-ap " ap") (+ x 4) (- y 96)))))))

(defn draw-players
  []
  (let [players @api/sorted-players]
    (dotimes [n 6]
      (let [[x y w h] (api/player-rect n)]
        (draw-player x (dec y) (nth players n nil))))))