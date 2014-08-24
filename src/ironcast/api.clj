(ns ironcast.api
  (:import (com.badlogic.gdx Gdx))
  (:require [ironcast.state :refer :all]
            [ironcast.internals.cam :as cam]
            [ironcast.pure.create :refer [try-create-world] :as create]
            [ironcast.pure.attr :as attr]
            [ironcast.pure.pos :as pos]
            [ironcast.pure.move :as move]
            [ironcast.pure.act :as act]
            [ironcast.pure.time :as time]
            [ironcast.pure.spell :as spell]
            [ironcast.io :as io]
            [ironcast.db :as db]
            [ironcast.tiled :as tiled]
            [ironcast.util :refer :all]
            [clj-tuple :refer [tuple]]
            [ironcast.event :as event]
            [ironcast.pure.vis :as vis]))

(defn setting
  ([key else]
   (get @settings key else))
  ([key]
   (setting key nil)))

(defn set-setting
  "Sets a setting"
  [setting val]
  (swap! settings assoc setting val))

(defn debug-on
  "Turn debug mode on"
  []
  (set-setting :debug? true))

(defn debug-off
  "Turn debug mode off"
  []
  (set-setting :debug? false))

(def delta
  "Deref to get the delta since last frame at this point in time"
  (>>
    (.. Gdx/graphics (getDeltaTime))))

(def fps
  "Deref to get the frames per second at this point in time"
  (>>
    (.. Gdx/graphics (getFramesPerSecond))))

(defn font
  "Find a font in the db - returning it if found"
  [key]
  (db/find-font @db key))

(defn sprite
  "Finds a sprite in the db - returning it if found"
  [key]
  (if (keyword? key)
    (db/find-sprite @db key)
    key))

(defn find-map
  [key]
  (db/find-map @db key))


(def default-font
  "Deref to get the default font, assumes an initialised game"
  (delay (font :default)))


(def block
  "Deref to get the `block` sprite - assumes an initialsed game."
  (delay (sprite :block)))

(def blank
  "Deref to get the `blank` sprite - assumes an initialised game"
  (delay (sprite :blank)))

(def debug?
  "Deref to determine whether we are in debug mode"
  (>> (setting :debug?)))

(def screen-width
  "Deref to get the current screen width"
  (>> (setting :width)))

(def screen-height
  "Deref to get the current screen height"
  (>> (setting :height)))

;; UI

(def ui-path
  (>>
    (let [world @world
          ui @ui]
      (when (time/player? world)
        (:path ui)))))

(def info
  (>>
    (:info @ui)))

(def top-left
  "Deref to get the current top-left co-ord in gl co-ordinates"
  (>> (tuple
        (/ @screen-width -2)
        (/ @screen-height 2))))

(def bottom-left
  "Deref to get the current bottom-left co-ord in gl co-ordinates"
  (>> (tuple
        (/ @screen-width -2)
        (/ @screen-height -2))))

(def bottom-right
  "Deref to get the current bottom-right co-ord in gl co-oridinates"
  (>> (tuple
        (/ @screen-width 2)
        (/ @screen-height -2))))

(def top-mid
  "Deref to get the current top-mid co-ord in gl co-ordinates"
  (>> (tuple
        (/ @screen-width -2)
        (/ @screen-height 2))))


(def game-rect
  "Deref to get the current `game-rect`, that is the view port inside the main ui that
   displays the in game action!"
  (>>
    (let [[x y] @top-left
          sh @screen-height
          sw @screen-width]
      (tuple
        (+ x 160)
        y
        (- sw 320)
        (- sh 228)))))


(defn player-rect
  [n]
  (let [[x y] @top-left
        [gx _ gw _] @game-rect
        gx (+ gx gw 32)
        x (if (< 2 n) gx x)]
    (tuple x (- y (* (int (mod n 3)) 192)) 128 160)))


(def mouse-pos
  "Deref to get the current mouse position"
  (>> (or (:mouse-pos @ui) (tuple 0 0))))

(def mouse-world-pos
  "Deref to get the current mouse world position (cam translated)"
  (>> (or (:mouse-world-pos @ui) (tuple 0 0))))

(def world-cell
  "Deref to get the current world cell at mouse pointer"
  (>> (or (:world-cell @ui) (tuple 0 0))))

(def screen-pos
  "Deref to get the current gl mouse position (centre 0,0)"
  (>> (let [[x y] @top-mid
            [mx my] @mouse-pos
            x (+ x mx)]
        (tuple x (- y my)))))

(defn mouse-in-game?*
  []
  (let [[x y w h] @game-rect]
    (pt-in-screen? x y w h @screen-pos)))

(def mouse-in-game?
  "Is the mouse captured by the game area?"
  (>> (when-not @info
        (mouse-in-game?*))))

(defn mouse-in?
  ([[x y w h]]
   (mouse-in? x y w h))
  ([x y w h]
   (pt-in-screen? x y (dec w) (dec h) @screen-pos)))

(defn click-in?
  "Are we clicking and if so is the click within the bounds supplied?"
  [x y w h]
  (and (@commands :select)
       (mouse-in? x y w h)))

(defn click-held-in?
  "Are we clicking and holding - if so is the click within the bounds supplied?"
  [x y w h]
  (and (@commands :select-hold)
       (mouse-in? x y w h)))

(defn mouse-in-player*
  []
  (-> (filter (fn [[i _]] (mouse-in? (player-rect i)))
              (map-indexed tuple (sort (attr/players @world))))
      first
      second))

(def mouse-in-player
  (>> (when-not @info
        (mouse-in-player*))))

;;LASSO

(def lasso
  "If lassoing returns the current lasso rectangle in screen co-ords"
  (>> (let [ui @ui]
        (when (:lassoing? ui)
          (-> ui :lasso :rect)))))

(def lasso-world
  "If lassoign returns the current lasso rectangle in world co-ords"
  (>>
    (let [ui @ui]
      (when (:lassoing? ui)
        (:lasso-world ui)))))


;;CAM

(defn move-cam
  "Move the cam to screen co-ordinates given by x and y"
  [x y]
  (cam/move! @cam x y))

(defn shift-cam
  "Shift the cam from its current position by given x and y"
  [x y]
  (cam/shift! @cam x y))

(def cam-pos (>> (cam/pos @cam)))

(defn jump-to-cell
  [x y]
  (move-cam (- (* 32 x) (/ @screen-width 2))
            (- (* (-> @world :height) 32) (* 32 y) (/ @screen-height 2))))

;;DB

(defn load-db
  []
  (reset! db (db/db {:atlas @atlas
                     :tiles (io/tiles)
                     :maps  (io/maps)
                     :fonts @fonts}))
  nil)


(def selected-menu
  (>> (:selected-menu @ui :game)))

(def selected-action-menu
  (>> (:selected-action-menu @ui :cast)))

(defn select-menu
  [menu]
  (swap! ui assoc :selected-menu menu))

(defn select-action-menu
  [menu]
  (swap! ui assoc :selected-action-menu menu))

(defn begin-cast
  [spell]
  (swap! ui assoc :casting spell))

(defn end-cast
  []
  (swap! ui dissoc :casting))

(def casting
  (>> (:casting @ui)))

(def not-casting
  (>> (nil? @casting)))

(def casting-single?
  (>> (-> @casting :spell-type (= :single))))

(def casting-tile?
  (>> (-> @casting :spell-type (= :tile))))


;;WORLD

(defn update-world
  [f & args]
  (dosync
    (apply alter world f args))
  nil)

(def world-height
  (>> (:height @world 32)))

(def mode
  (>> (:mode @world :real)))

(def real?
  (>> (= @mode :real)))

(def turns?
  (>> (time/turns? @world)))

(def enemy?
  (>> (= @mode :enemy)))

(def player?
  (>> (= @mode :player)))

(def combat?
  (>> (time/combat? @world)))

(defn real
  []
  (update-world time/real))

(defn combat
  []
  (update-world time/combat))

(defn peace
  []
  (update-world time/peace))

(defn flip-turn
  []
  (update-world time/flip))

(defn flip-mode
  []
  (update-world time/flip-mode))

(defn solid?
  [pt]
  (pos/solid-at? @world pt))

(defn not-solid?
  [pt]
  (pos/not-solid-at? @world pt))

(defn aware-of
  [pt]
  (vis/aware-of @world pt))

(defn creature-aware-of-at
  [pt]
  (let [w @world]
    (first (filter #(attr/creature? w %) (vis/aware-of w pt)))))

(defn create
  [try-f & args]
  (dosync
    (let [w @world
          [new-w id] (apply create/try-create w try-f args)]
      (ref-set world new-w)
      id)))

(defn creature
  [pt & opts]
  (apply create create/creature pt opts))

(defn item
  [flags & attrs]
  (apply create create/item flags attrs))

(def items
  (>> (-> @world :with-flag :item)))

(defn pos
  [ent]
  (pos/pos @world ent))

(defn at
  [pt]
  (pos/at @world pt))

(defn creature-at
  [pt]
  (pos/creature-at @world pt))

(defn jump-to-entity
  [ent]
  (let [[x y :as p] (pos ent)]
    (when p
      (jump-to-cell x y))))


;; WORLD - SELECTION
(def selected
  (>> (attr/selected @world)))

(defn selected?
  [ent]
  (attr/selected? @world ent))

(defn selectable-at?
  [pt]
  (pos/selectable-at? @world pt))

(def selectable-at-mouse?
  (>> (selectable-at? @world-cell)))

(defn unselect-all
  []
  (update-world attr/unselect-all))

(defn select-many-in
  [rect]
  (update-world pos/select-in rect))

(defn select
  [ent]
  (update-world attr/select ent))

(defn select-only
  [ent]
  (update-world attr/select-only ent))

(defn select-at
  [pt]
  (update-world pos/select-at pt))

(defn select-at-mouse
  []
  (select-at @world-cell))

(defn select-only-at
  [pt]
  (update-world pos/select-only-at pt))

(defn select-only-at-mouse
  []
  (select-only-at @world-cell))

(defn unselect
  [ent]
  (update-world attr/unselect ent))

(def players
  (>> (attr/players @world)))

(def sorted-players
  (>> (attr/sorted-players @world)))

(defn attr
  ([ent attribute]
   (attr/attr @world ent attribute))
  ([ent attribute else]
   (attr/attr @world ent attribute else)))

(defn all-attrs
  [ent]
  (attr/all @world ent))

(defn all-flags
  [ent]
  (attr/all-flags @world ent))

(defn other-actions
  [ent pt]
  (filter #(act/could? @world ent pt %) act/other-actions))

(defn default-actions
  [ent pt]
  (filter #(act/can? @world ent pt %) act/default-actions))

(defn default-action
  [ent pt]
  (first (default-actions ent pt)))

(defn can-afford?
  ([action ent]
   (act/can-afford? @world ent action))
  ([action]
   (can-afford? action (first @selected))))

(defn act-applies?
  [action ent pt]
  (act/applies? @world ent pt action))

(defn can-act?
  [action ent pt]
  (act/can? @world ent pt action))

(defn can-act-at-mouse?
  ([action]
   (can-act-at-mouse? action (first @selected)))
  ([action ent]
   (act/can? @world ent @world-cell action)))

(defn can-act-at-player?
  ([action]
   (when-let [player @mouse-in-player]
     (can-act-at-player? action player)))
  ([action player]
   (when-let [pt (pos/pos player)]
     (can-act? action (first @selected) pt))))

(def current-act-aoe
  (>>
    (let [world @world
          ent (first @selected)
          action @casting
          at @world-cell]
      (and world first action at
           (act/aoe world ent at action)))))

(def current-act-descr
  (>> (when-let [act @casting]
         (act/descr @world
                    (first @selected)
                    @world-cell
                    act))))

(defn do-act
  [action]
  (event/put-act! action))

(defn act
  [action ent pt]
  (do-act (act/prepare @world ent pt action)))

(defn act-at-mouse
  ([action]
   (act-at-mouse action (first @selected)))
  ([action ent]
    (act action ent @world-cell)))

(defn act-at-player
  ([action]
   (when-let [player @mouse-in-player]
     (act-at-player action player)))
  ([action player]
   (when-let [pt (pos/pos player)]
     (act action (first @selected) pt))))

(def player-at-mouse
  (>> (pos/player-at @world @world-cell)))

(def enemy-at-mouse
  (>> (pos/enemy-at @world @world-cell)))

(def can-cast-at-mouse?
  (>> (let [spell @casting
            world @world
            caster (first (attr/selected world))
            pt @world-cell]
        (when (and spell caster)
          (case (:spell-type spell)
            :single (act/can? world caster pt spell)
            true)))))


(def current-spells
  "Return the list of spells for the current selected entity"
  (>> [spell/magic-missiles spell/sleep-touch]))

(def dwarves ["Sleepy" "Bashful" "Grumpy" "Dopey" "Doc" "Sneezy"])

(defn dwarf
  [name pt]
  (creature pt #{:player}
            :sprite (sprite :dwarf-male)
            :name name
            :descr "A Dwarf"))

(defn leather-armour
  []
  (item #{:torso}
        :sprite :leather-armour
        :equip-sprite :leather-armour-equip))

(defn dwarfs
  "Creates a test party"
  [pt]
  (doseq [[name pt] (map tuple dwarves (flood pt not-solid?))]
    (let [ent (dwarf name pt)
          armour (leather-armour)
          sword (item #{:hand}
                      :sprite :sword
                      :equip-sprite :sword-equip)]
      (update-world attr/equip ent armour)
      (update-world attr/equip ent sword))))

(defn goblin
  [pt]
  (creature pt #{:enemy :ai}
            :sprite (sprite :goblin-slave)
            :name "Goblin"
            :descr "A goblin"))

(defn goblins
  [n pt]
  (doseq [pt (take n (flood pt not-solid?))]
    (goblin pt)))

(defn seed
  []
  (long (rand Long/MAX_VALUE)))

(defn test-world
  []
  (let [[new-world success?] (try-create-world
                               @db
                               (seed)
                               (find-map :test))]
    (if success?
      (do (dosync (ref-set world
                           new-world))
          (dwarfs [17 18])
          (goblins 8 [16 21])
          "success")
      "failed")))

(defn reload-all
  []
  (load-db)
  (test-world))

(defn blat-world
  []
  (update-world (constantly nil)))