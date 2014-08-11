(ns ironcast.ui
  (:require [ironcast.internals.input.lasso :as lasso]
            [ironcast.internals.cam :as cam]
            [ironcast.util :refer :all]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.attr :as attr]
            [ironcast.pure.pos :as pos]
            [ironcast.pure.move :as move]))

(defn world-cell
  ([world [x y]]
   (world-cell world x y))
  ([world x y]
   (tuple (int (/ x 32))
          (- (-> world :height)
             (int (Math/ceil (/ y 32)))))))

(defn world-lasso
  [world lasso]
  (let [rect (:rect (or lasso lasso/initial))
        mvd (move-pt rect (world-cell world rect))]
    (-> (div-rect mvd 1 1 32 32)
        (int-tuple)
        (explode-rect 1))))

(defn with-lasso
  [ui input cam world]
  (let [lasso (update (:lasso input)
                      :rect #(cam/unproject-rect cam %))]
    (assoc ui
      :lasso lasso
      :lasso-world (world-lasso world lasso)
      :lassoing? (lasso/lassoing? lasso))))

(defn update-ui
  [ui input cam world]
  (let [mouse-pos (:mouse-pos input)
        mouse-world-pos (cam/unproject cam mouse-pos)]
    (-> (assoc ui :mouse-world-pos mouse-world-pos
                  :mouse-pos mouse-pos
                  :world-cell (world-cell world mouse-world-pos))
        (with-lasso input cam world))))

(defn path-cell-sprite
  [world ent [pos _ total]]
  (let [sprite (attr/path-sprite world ent total)]
    (tuple pos sprite)))

(defn find-selected-path
  [ui world]
  (let [to (:world-cell ui)]
    (when-let [selected (first (attr/selected world))]
      (when-let [pos (pos/pos world selected)]
        (->> (move/find-path-for world selected to)
             (cost-path pos)
             (map #(path-cell-sprite
                    world
                    selected
                    %)))))))
(defn should-path?
  [ui]
  (let [to (:world-cell ui)
        current (:path ui)]
    (not= (last current) to)))


(def menu-buttons
  [[0 :game :combat-options "Game"]
   [1 :stats :stats "Stats"]
   [2 :inventory :inventory "Inventory"]
   [3 :spellbook :spells "Spellbook"]
   [4 :map :map "Map"]
   [5 :options :options "Options"]])

(def action-buttons
  [[0 :cast :mouse-magic "Spells & Abilities"]
   [1 :use :inventory "Use Quick Items"]])