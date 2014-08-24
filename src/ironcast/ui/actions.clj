(ns ironcast.ui.actions
  (:require [ironcast.ui.base :refer :all]
            [ironcast.internals.gfx :as gfx]
            [ironcast.api :as api]
            [ironcast.pure
             [attr :refer :all]]
            [clojure.string :as str]
            [clj-tuple :refer [tuple]]
            [ironcast.state :as state]
            [ironcast.pure.act :as act]))


(defn textify
  [index world ent pt act]
  (str (inc index) ". " (act/show world ent pt act)))


(defn draw-action
  [world ent action font text x y]
  (gfx/with-font-color font (if (act/can-afford? world ent action)
                              :off-white
                              :grey)
                       (gfx/draw-text! font text x y)))

(defn draw-actions
  [world ent actions]
  (let [[x y] @api/screen-pos
        pt @api/world-cell
        strings (map-indexed #(textify %1 world ent pt %2) actions)
        bounds (map #(gfx/text-bounds @api/default-font %) strings)
        w (apply max (map first bounds))
        n (count actions)
        font @api/default-font]
    (gfx/draw-box! @api/blank (+ x 32) y (+ w 10) (*  n 22) :transparent-black :yellow)
    (loop [[act & actions] actions
           [text & strings] strings
           [[_ h] & bounds] bounds
           lh 4]
      (when (and act text h)
        (draw-action world ent act font text (+ x 36) (- y lh))
        (recur actions strings bounds (+ lh (int h)))))))

(defn draw
  []
  (when-not @api/casting
    (let [ent (first @api/selected)
          pt @api/world-cell
          world @state/world
          actions (api/other-actions ent pt)]
      (when (and ent (seq actions))
        (draw-actions world ent actions)))))

