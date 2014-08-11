(ns ironcast.internals.gfx
  (:import (com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont BitmapFont$TextBounds TextureRegion SpriteCache)
           (com.badlogic.gdx Gdx)
           (com.badlogic.gdx.graphics GL20 Texture Color OrthographicCamera)
           (com.badlogic.gdx.math Matrix4))
  (:require [ironcast.util :refer :all]
            [clj-tuple :refer [tuple]]
            [clj-tiny-grid.core :as grid]))

(def ^:dynamic ^SpriteBatch *batch* nil)

(def white (colors :white))
(def identity-matrix
  (Matrix4.))

(defn set-cam!
  [^OrthographicCamera cam]
  (.update cam)
  (.setProjectionMatrix *batch* (.combined cam)))

(defn release-cam!
  [^OrthographicCamera cam]
  (.setTransformMatrix *batch* identity-matrix)
  (.setProjectionMatrix *batch* (.projection cam)))

(defn begin!
  []
  (when *batch*
    (.begin *batch*)))

(defn end!
  []
  (when *batch*
    (.end *batch*)))

(defn clear!
  []
  (. Gdx/gl glClearColor 0 0 0 0)
  (. Gdx/gl glClear GL20/GL_COLOR_BUFFER_BIT))

(defn fit-screen!
  []
  (let [w (. Gdx/graphics getWidth)
        h (. Gdx/graphics getHeight)]
    (. Gdx/gl20 (glViewport 0 0 w h))))

(defn font-color!
  ([^BitmapFont font r g b a]
   (.setColor font r g b a))
  ([^BitmapFont font ^Color color]
   (.setColor font color)))

(defn color!
  ([r g b a]
   (.setColor *batch* r g b a))
  ([^Color color]
   (.setColor *batch* color)))

(defonce last-exc (atom nil))
(defmacro with-batch
  [batch & forms]
  `(binding [*batch* ~batch]
     (try
       (do
         (begin!)
         ~@forms)
       (catch Throwable e#
         (reset! last-exc e#))
       (finally
         (end!)))))

(defmacro with-color
  [color & forms]
  `(do
     (color! (colors ~color))
     ~@forms
     (color! white)))

(defn get-font-color
  [^BitmapFont font]
  (.getColor font))

(defmacro with-font-color
  [font color & forms]
  `(do
     (let [color# (get-font-color ~font)]
       (font-color! ~font (colors ~color))
       ~@forms
       (font-color! ~font color#))))


(defn draw-region!
  ([^TextureRegion tex ^double x ^double y]
   (.draw *batch* tex x y ))
  ([^TextureRegion tex x y w h]
   (.draw *batch* tex
          ^float x
          ^float y
          ^float w
          ^float h)))

(defn text-bounds
  [^BitmapFont font str]
  (let [^BitmapFont$TextBounds b (.getMultiLineBounds font str)]
    (tuple (.width b)
           (.height b))))



(defn draw-text!
  [^BitmapFont font str ^double x ^double y]
  (.drawMultiLine font *batch* str x y))

(defn draw-text-wrapped!
  [^BitmapFont font str x y width]
  (.drawWrapped font *batch* str x y width))

(defn draw-sprite!
  ([{tex :region} x y w h]
   (draw-region! tex x y w h))
  ([{tex :region} ^double x ^double y]
   (draw-region! tex x y)))


(defn draw-border!
  ([blank [x y w h] thickness]
   (draw-border! blank x y w h thickness))
  ([blank x y w h thickness]
   ;;top
   (draw-sprite! blank x (- y thickness) w thickness)
   ;;right
   (draw-sprite! blank (- (+ x w) thickness) (- y h) thickness h)
   ;;bottom
   (draw-sprite! blank x (- y h ) w thickness)
   ;;left
   (draw-sprite! blank x (- y h) thickness h)))


(defn draw-box!
  ([blank [x y w h] back-color color]
   (draw-box! blank x y w h back-color color))
  ([blank x y w h back-color color]
   (with-color back-color
     (draw-sprite! blank x (- y h) w h))
   (with-color color
     (draw-border! blank x y w h 1))))

(defn draw-backed-box!
  ([blank [x y w h] color]
   (draw-backed-box! blank x y w h color))
  ([blank x y w h color]
   (draw-box! blank x y w h :black color)))

(defn draw-resource-bar!
  [blank x y w h color min max]
  (let [perc (clojure.core/min (/ min max) 1)
        perc (clojure.core/max 0 perc)]
    (with-color :black
      (draw-sprite! blank x (- y h) w h))
    (with-color :dark-yellow
      (draw-border! blank (dec x) (inc y) (+ w 2) (inc h) 1))
    (with-color color
      (draw-sprite! blank x (- y h) (* w perc) h))))

(defn draw-sprite-container!
  [entity x y]
  (when-let [spr (:sprite entity)]
    (draw-sprite! spr x y)))

