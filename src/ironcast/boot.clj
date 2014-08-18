(ns ironcast.boot
  (:require [ironcast.core :refer :all]
            [ironcast.api :as api]))


(defn go-slow
  []
  (api/set-setting :max-fps 15))

(defn go-fast
  []
  (api/set-setting :max-fps 0))


(go)