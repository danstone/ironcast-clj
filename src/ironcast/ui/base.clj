(ns ironcast.ui.base
  (:require [ironcast.internals.gfx :as gfx]
            [ironcast.api :as api]
            [ironcast.util :refer :all]
            [ironcast.state :as state]))

(defn scroll-down
  [i]
  (if i
    (inc i)
    1))

(defn scroll-up
  [i]
  (if (and i (pos? i))
    (dec i)
    0))

(defn draw-text!
  [text x y]
  (gfx/draw-text! @api/default-font text x y))

(defn draw-blank!
  [x y width height]
  (gfx/draw-sprite! @api/blank x y width height))

(defn draw-block!
  ([x y]
   (gfx/draw-sprite! @api/block x y))
  ([x y width height]
   (gfx/draw-sprite! @api/block x y width height)))

(defn draw-blocks!
  ([[x y w h]]
   (draw-blocks! x y w h))
  ([x y w h]
   (let [y (- y 32)]
     (dotimes [xi w]
       (dotimes [yi h]
         (draw-block! (+ x (* 32 xi))
                      (- y (* 32 yi))))))))

(defn draw-hover-border!
  [x y w h]
  (let [color (if (api/mouse-in? x y w h)
                :light-yellow
                :off-white)]
    (gfx/draw-backed-box! @api/blank x y w h color)))

(defn draw-hover-sprite!
  [sprite x y w h bw bh]
  (let [color (if (api/mouse-in? x (+ y bh) bw bh)
                :white
                :off-white)]
    (gfx/with-color
      color
      (gfx/draw-sprite! sprite x y w h))))

(defn draw-hover-text!
  [font text x y bx by bw bh]
  (let [color (if (api/mouse-in? bx (+ by bh) bw bh)
                :light-green
                :off-white)]
    (gfx/with-font-color
      font color
      (gfx/draw-text! font text x y))))

(defn draw-lasso!
  []
  (when (= @api/mode :real)
    (when-let [lasso @api/lasso]
      (gfx/with-color
        :green
        (gfx/draw-border! @api/blank lasso 1)))))


(defn draw-aoe!
  []
  (when (or @api/player? @api/real?)
    (when-let [aoe (:casting-aoe @state/ui)]
      (let [height @api/world-height
            spr (api/sprite :ministar)
            caster (first @api/selected)
            spell @api/casting]
        (when (and caster spell)
          (doseq [[wx wy :as pt] aoe
                  :let [x (* wx 32)
                        y (* (- height wy 1) 32)]]
            (if (api/act-applies? spell caster pt)
              (gfx/with-color :light-yellow
                (gfx/draw-sprite! spr
                                  (+ x 12)
                                  (- y 12)))
              (gfx/with-color :off-white
                (gfx/draw-sprite! spr
                                  (+ x 12)
                                  (- y 12))))))))))

(defn draw-world-text!
  []
  (let [height @api/world-height
        font @api/default-font]
    (doseq [{:keys [text color pos time]} @state/world-text
            :let [[x y] pos
                  [w _] (gfx/text-bounds font text)
                  x (+ (* x 32) 16 (/ w -2))
                  y (* (+ height (- y) time) 32)]]
      (gfx/with-font-color
        font color
        (gfx/draw-text! font text x y)))))


(defn draw-path!
  []
  (when-let [path @api/ui-path]
    (let [height (:height @state/world)]
      (doseq [[[x y] sprite] path
              :let [x (* x 32)
                    y (* (- height y 1) 32)]]
        (gfx/draw-sprite! (api/sprite sprite) x y)))))

(defn draw-button
  [sprite x y w h]
  (draw-hover-border! x y w h)
  (draw-hover-sprite! sprite x (- y 32) 32 32 w 32))

(defn draw-text-button
  [sprite text x y w h]
  (draw-button sprite x y w h)
  (draw-hover-text! @api/default-font text
                    (+ x 36)
                    (- y 8) x (- y 32) w h))

(defn draw-selected-button
  [sprite x y w h]
  (gfx/with-color :light-green
    (gfx/draw-border! @api/blank x y w h 1))
  (gfx/draw-sprite! sprite x (- y 32) 32 32))

(defn draw-selected-text-button
  [sprite text x y w h]
  (draw-selected-button sprite x y w h)
  (gfx/draw-text! @api/default-font text
                  (+ x 36)
                  (- y 8)))

(defn cap-scroll!
  [lines key]
  (let [scroll (get @state/ui key 0)]
    (swap! state/ui assoc key (max 0 (min (count lines) scroll)))))

(defn draw-scroll!
  [x y h key]
  (let [up (api/sprite :scroll-up)
        down (api/sprite :scroll-down)
        x (+ (- x 4) (* 10 32))
        y (+ y 6)]

    (draw-hover-sprite! up x (- y 32) 32 32 32 32)
    (draw-hover-sprite! down x (+ y h) 32 32 32 32)

    (when (api/click-in? x y 32 32)
      (swap! state/ui update key scroll-up))
    (when (api/click-held-in? x y 32 32)
      (swap! state/ui update key - (* 8 @api/delta)))
    (when (api/click-in? x (+ y h 32) 32 32)
      (swap! state/ui update key scroll-down))
    (when (api/click-held-in? x (+ y h 32) 32 32)
      (swap! state/ui update key + (* 8 @api/delta)))))
