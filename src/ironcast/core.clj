(ns ironcast.core
  (:import (com.badlogic.gdx ApplicationListener Gdx)
           (com.badlogic.gdx.backends.lwjgl LwjglApplicationConfiguration LwjglApplication)
           (com.badlogic.gdx.graphics Texture OrthographicCamera)
           (com.badlogic.gdx.graphics.g2d BitmapFont SpriteBatch TextureAtlas SpriteCache)
           (java.util.concurrent ConcurrentLinkedQueue)
           (com.badlogic.gdx.files FileHandle))
  (:require [ironcast.frame :refer [frame]]
            [ironcast.io :as io]
            [ironcast.api :refer :all]
            [ironcast.state :as state]
            [ironcast.task :as task]))


(set! *unchecked-math* true)

(defn init
  []
  (reset! state/cam (OrthographicCamera.
                      (setting :width 1024)
                      (setting :height 768)))
  (reset! state/batch (SpriteBatch.))
  (reset! state/atlas (io/atlas "main.pack"))
  (reset! state/fonts (io/fonts))
  (load-db)
  (test-world)
  (task/start-all))

(def app
  {:create init
   :render #'frame})

(defn map->listener
  "pass a map of keys to create a listaener"
  [{:keys [create render]}]
  (proxy
      [ApplicationListener]
      []
    (pause [])
    (resume [])
    (dispose [])
    (create []
      (create))
    (render []
      (render))
    (resize [x y])))

(def listener
  (map->listener app))

(defn go
  []
  (let [cfg (LwjglApplicationConfiguration.)]
    (set! (. cfg width) (setting :width 1024))
    (set! (. cfg height) (setting :height 768))
    (set! (. cfg fullscreen) (setting :fullscreen? false))
    (set! (. cfg title) "Ironcast 0.1.0")
    (set! (. cfg vSyncEnabled) (setting :vsync? false))
    (set! (. cfg foregroundFPS) (setting :max-fps 60))
    (set! (. cfg backgroundFPS) (setting :max-fps 60))
    (set! (. cfg resizable) false)
    (future (LwjglApplication. listener cfg))))

