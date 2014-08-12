(ns ironcast.ui.controls
  (:require [ironcast.ui.base :refer :all]
            [ironcast.util :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.api :as api]
            [ironcast.state :as state]
            [ironcast.ui :as ui]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.time :as time]))

(defn turn-timer-sprite
  []
  (let [world @state/world]
    (cond
      (time/combat? world) (api/sprite :next-turn)
      (time/turns? world) (api/sprite :real-mode)
      :else (api/sprite :combat-mode))))

(defn turn-timer-text
  []
  (let [world @state/world
        mode (:mode world :real)]
    (cond
      (time/combat? world) "Next\nTurn"
      (= mode :real) "Combat\nMode"
      :else "End\nCombat")))

(defn handle-turn-timer-click
  []
  (if @api/combat?
    (api/flip-turn)
    (api/flip-mode)))

(defn draw-enemy-turn-timer
  [x y]
  (gfx/with-color :grey
    (gfx/draw-sprite! (turn-timer-sprite) x y 64 64)
    (gfx/draw-text! @api/default-font "Enemy\nTurn" (+ x 66) (+ y 50))))

(defn draw-player-turn-timer
  [x y]
  (draw-hover-sprite! (turn-timer-sprite)
                      x y 64 64
                      128 64)
  (draw-hover-text! @api/default-font
                    (turn-timer-text)
                    (+ x 66) (+ y 50)
                    x y
                    128 64)
  (when (api/click-in? x (+ y 64) 128 64)
    (handle-turn-timer-click)))

(defn draw-turn-timer
  []
  (let [[x y] @api/bottom-right
        x (- x 128)
        y (+ y 32)]
    (if (and @api/combat? @api/enemy?)
      (draw-enemy-turn-timer x y)
      (draw-player-turn-timer x y))))


(defn draw-log-message
  [font message x y]
  (if (string? message)
    (gfx/draw-text! font message x y)
    (gfx/with-font-color font (first message)
                         (gfx/draw-text! font (second message) x y))))


(defn draw-log
  [x y max-lines]
  (let [log @state/log
        log (drop (int (:log-scroll @state/ui 0)) log)
        [_ by] @api/bottom-left
        font @api/default-font]
    (draw-scroll! x y (- by y) :log-scroll)
    (cap-scroll! log :log-scroll)
    (doall (map-indexed
             (fn [i m]
               (draw-log-message font m x (- y (* i 18))))
             (take max-lines log)))))

(defn draw-casting-descr
  [x y max-lines]
  (let [descr nil #_@api/current-spell-descr
        descr (drop (int (:casting-scroll @state/ui 0)) descr)
        [_ by] @api/bottom-left
        font @api/default-font]
    (draw-scroll! x y (- by y) :casting-scroll)
    (cap-scroll! descr :casting-scroll)
    (doall (map-indexed
             (fn [i m]
               (draw-log-message font m x (- y (* i 18))))
             (take max-lines descr)))))

(def descr-rect
  (>>
    (let [[x y w h] @api/game-rect
          sh @api/screen-height
          y (int (- y h 32))
          x (int (+ x w (- (* 11 32))))
          h (int (- sh h 32))]
      (tuple x y w h))))

(defn draw-descr-bar
  []
  (let [[x y w h] @descr-rect
        y (int (- y 6))
        x (int (+ 4 x))
        max (int (/ h 18))]
    (cond
      false #_@api/casting (draw-casting-descr x y max)
      :otherwise (draw-log x y max))))


(def menu-button-rect
  (>>
    (let [[x y _ h] @api/game-rect
          x (- x 160)
          y (* 32 (int (/ (- y h 64) 32)))]
      (tuple x y 128 (* 5 32)))))

(defn draw-menu-buttons
  []
  (let [[x y] @menu-button-rect
        selected @api/selected-menu]
    (doseq [[i name sprite text] ui/menu-buttons
            :let [y (- y (* i 32) 1)]]
      (if (= selected name)
        (draw-selected-text-button
          (api/sprite sprite)
          text
          x y 128 32)
        (draw-text-button
          (api/sprite sprite)
          text
          x y 128 32))
      (when (api/click-in? x y 128 32)
        (api/select-menu name)))))

(defn draw-action-buttons
  []
  (let [[x y _ h] @api/game-rect
        y (* 32 (int (/ (- y h 64) 32)))
        selected @api/selected-action-menu]
    (doseq [[i name sprite text] ui/action-buttons
            :let [x (+ x (* i 160))]]
      (if (= selected name)
        (draw-selected-text-button
          (api/sprite sprite)
          text
          x y 160 32)
        (draw-text-button
          (api/sprite sprite)
          text
          x y 160 32))
      (when (api/click-in? x y 160 32)
        (api/select-action-menu name)))))

(defn draw-spell
  [x y spell]
  (let [color (cond
                (not (api/can-afford? spell)) :grey
                (api/mouse-in? x (+ y 32) 32 32) :white
                :else :off-white)]
    (gfx/with-color
      color
      (gfx/draw-sprite! (api/sprite (:sprite spell)) x y 32 32))))

(defn draw-spell-hover
  [x y spell]
  (let [text (:text spell "???")
        [w h] (gfx/text-bounds @api/default-font text)]
    (gfx/draw-box! @api/blank (+ x 32) y (+ w 16) (+ h 16)
                   :transparent-black
                   :yellow)
    (draw-text! text (+ x 40) (- y 8))))


(defn draw-spells
  [x y]
  (let [spells (map-indexed tuple @api/current-spells)]
    (doseq [[i spell] spells]
      (draw-spell (+ x (* i 32)) y spell)
      (when (api/mouse-in? (+ x (* i 32)) (+ y 32) 32 32)
        (draw-spell-hover (+ x (* i 32)) y spell))
      (when (and (api/click-in? (+ x (* i 32)) (+ y 32) 32 32)
                 (api/can-afford? spell))
        (api/begin-cast spell)))))

(defn draw-action-bar
  []
  (when (first @api/selected)
    (let [[x y _ h] @api/game-rect
          y (* 32 (int (/ (- y h 96) 32)))]
      (when (= :cast @api/selected-action-menu)
        (draw-spells x (- y 32))))))