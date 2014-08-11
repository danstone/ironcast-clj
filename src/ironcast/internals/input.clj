(ns ironcast.internals.input
  (:import (com.badlogic.gdx Input$Keys Gdx Input Input$Buttons)
           (org.lwjgl.input Mouse Cursor)
           (org.lwjgl BufferUtils))
  (:require [clojure.string :as str]
            [ironcast.internals.input.lasso :as lasso]
            [clj-tuple :refer [tuple]]))

(defprotocol IInput
  (get-x [this])
  (get-y [this])
  (key-pressed? [this key])
  (button-pressed? [this button]))

(defrecord GdxInput []
  IInput
  (get-x [this] (.. Gdx/input (getX)))
  (get-y [this] (.. Gdx/input (getY)))
  (key-pressed? [this key] (.. Gdx/input (isKeyPressed key)))
  (button-pressed? [this button] (.. Gdx/input (isButtonPressed button))))


(defn mouse-pos
  "Find the current mouse position for the given IInput"
  [input]
  (tuple (get-x input)
         (get-y input)))

(def key-up?
  (complement key-pressed?))

(def button-up?
  (complement button-pressed?))

(def key-nums
  (into {}
        (for [n (range 0 10)
              :let [i (+ 7 n)]]
          [(keyword (str "num" n)) i])))

(def key-chars
  (into {}
        (for [n (range 0 26)
              :let [i (+ 65 n)
                    ci (+ 29 n)]]
          [(keyword (str/lower-case (str (char i)))) ci])))

(def key-map
  (merge {:any -1
          :lshift Input$Keys/SHIFT_LEFT
          :rshift Input$Keys/SHIFT_RIGHT
          :esc Input$Keys/ESCAPE
          :space Input$Keys/SPACE
          :f1 Input$Keys/F1
          :f2 Input$Keys/F2
          :f3 Input$Keys/F3
          :f4 Input$Keys/F4
          :f5 Input$Keys/F5
          :f6 Input$Keys/F6
          :f7 Input$Keys/F7
          :f8 Input$Keys/F8
          :f9 Input$Keys/F9
          :f10 Input$Keys/F10
          :f11 Input$Keys/F11
          :f12 Input$Keys/F12}
         key-nums
         key-chars))

(def button-map
  {:left Input$Buttons/LEFT
   :right Input$Buttons/RIGHT})


(defn keys-pressed
  [input]
  (for [[key real-key] key-map
        :when (key-pressed? input real-key)]
    key))

(defn buttons-pressed
  [input]
  (for [[button real-button] button-map
        :when (button-pressed? input real-button)]
    button))

(defn get-state
  [input]
  {:pressed (set (concat (buttons-pressed input) (keys-pressed input)))
   :mouse-pos (mouse-pos input)})

(defn hit
  [prev-state input]
  (for [p (:pressed prev-state)
        :when (or (when-let [k (get key-map p)] (key-up? input k))
                  (when-let [b (get button-map p)] (button-up? input b)))]
    p))

(defn lasso
  [old-state input-state]
  (if (-> input-state :pressed (get :left))
    (lasso/update (:lasso old-state) (:mouse-pos input-state))
    lasso/initial))


(defn cycle-state
  [old-state new-state input]
  (assoc new-state
    :last (dissoc old-state
                  :last
                  :hit)
    :hit (set (hit old-state input))
    :lasso (lasso old-state new-state)))

(defn hold
  [key]
  (tuple :hold key))

(defn press
  [key]
  (tuple :press key))

(defn commands-hit
  [input-state commands]
  (let [{down? :pressed
         hit? :hit} input-state]
    (for [[command [type thing]] commands
          :when (case type
                  :hold (get down? thing)
                  :press (get hit? thing))]
      command)))

(defonce hw-cursor (atom nil))
(defn hide-hw-cursor
  []
  (if @hw-cursor
    (when (Mouse/isInsideWindow)
        (Mouse/setNativeCursor @hw-cursor))
    (if (Mouse/isCreated)
      (let [min (Cursor/getMaxCursorSize)
            tmp (BufferUtils/createIntBuffer (* min min))]
        (reset! hw-cursor
                (Cursor. min min (/ min 2) (/ min 2) 1 tmp nil))))))

