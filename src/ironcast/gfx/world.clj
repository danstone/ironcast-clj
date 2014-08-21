(ns ironcast.gfx.world
  (:require [ironcast.internals.gfx :refer :all]
            [ironcast.pure.attr :refer :all]
            [ironcast.pure.pos :refer :all]
            [ironcast.pure.vis :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.api :as api]
            [ironcast.state :as state]))

(defn draw-seq
  "Take a sequence of entities and draw them
   using the draw-fn provided."
  ([world seq]
   (draw-seq world #(when-let [spr (attr world %3 :sprite)]
                     (draw-sprite! spr %1 %2)) seq))
  ([world drawfn seq]
    (let [h (:height world 32)
          shift @state/ent-shift]
      (doseq [e seq
              :let [pos (pos world e)]
              :when (and pos
                         (explored? world pos))
              :let [[x y] pos
                    [_ x2 y2] (get shift e)
                    x (+ (* x 32) (or x2 0))
                    y (- (* (- h y 1) 32) (or y2 0))]]
        (cond
          (visible? world pos) (with-color :white (drawfn x y e))
          (creature? world e) nil
          :else (with-color :grey (drawfn x y e)))))))

(defn draw-by-flag
  "Draw all entities with `flag` via draw-seq"
  [world flag]
  (draw-seq world (-> world :with-flag flag)))

(defn draw-circles
  "Draw entity circles with `color` for all
   entities with `flag`"
  [world flag color]
  (let [spr (api/sprite :selection)]
    (draw-seq world
              (fn [x y e]
                (with-color color (draw-sprite! spr x y)))
              (-> world :with-flag flag))))


(defn draw-missiles
  [world]
  (let [height (:height world)]
    (doseq [{:keys [from sprite]} (:missiles world)]
      (let [[x y] from]
        (draw-sprite! (api/sprite sprite)
                      x
                      (- (* 32 height) y 32))))))

(defn draw-creature
  [world x y ent]
  (let [base (attr world ent :sprite)
        equip (attr world ent :equip)
        equip-sprites (keep #(attr world % :equip-sprite) equip)]
    (doseq [sprite (cons base equip-sprites)]
      (draw-sprite! (api/sprite sprite) x y 32 32))))

(defn draw-creatures
  [world]
  (draw-seq world
            (partial draw-creature world)
            (creatures world)))

(defn draw-world
  "Draws the world!"
  [world]
  (draw-by-flag world :floor)
  (draw-by-flag world :wall)
  (draw-by-flag world :decor)
  (draw-circles world :enemy :red)
  (draw-circles world :selected :green)
  (draw-creatures world)
  (draw-missiles world))

