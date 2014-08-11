(ns ironcast.internals.cam
  (:import (com.badlogic.gdx.graphics OrthographicCamera Camera)
           (com.badlogic.gdx.math Vector3))
  (:require [clj-tuple :refer [tuple]]))

(defn pos
  [^Camera cam]
  (.position cam))

(defn move!
  [^OrthographicCamera cam x y]
  (.set (.position cam)
        (+ x (/ (.viewportWidth cam) 2))
        (+ y (/ (.viewportHeight cam) 2))
        0))

(defn shift!
  [^OrthographicCamera cam x y]
  (.translate cam x y))

(defn project
  ([cam [x y]]
   (project cam x y))
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.project cam vec)
     (tuple
       (int (.x vec))
       (int (.y vec))))))

(defn unproject
  ([cam [x y]]
   (unproject cam x y))
  ([^Camera cam x y]
   (let [vec (Vector3. x y 1)]
     (.unproject cam vec)
     (tuple
       (int (.x vec))
       (int (.y vec))))))

(defn unproject-rect
  [cam [x y w h]]
  (let [[x2 y2] (unproject cam x y)]
    (tuple x2 y2 w h)))

(defn top-left
  [^OrthographicCamera cam]
  (tuple (- (/ (.viewportWidth cam) 2))
         (/ (.viewportHeight cam) 2)))