(ns ironcast.pure.effect
  (:require [ironcast.util :refer :all]))

(defn missile
  [from to]
  {:type :missile
   :from   (-> (mult-ptn from 32) (add-pt 16 16))
   :to     (-> (mult-ptn to 32) (add-pt 16 16))})

(defn add-missile
  [world missile]
  (update world :missiles conj missile))

