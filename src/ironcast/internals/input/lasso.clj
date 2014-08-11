(ns ironcast.internals.input.lasso
  (:require [clj-tuple :refer [tuple]]))

(def initial
  {:last nil
   :rect (tuple 0 0 0 0)})

(defn initial?
  [{last :last :as lasso}]
  (or (nil? lasso)
      (nil? last)))

(defn lassoing?
  [{[_ _ w h] :rect :as lasso}]
  (and (not (initial? lasso))
       (> w 10)
       (> h 10)))

(defn update
  [lasso mouse-pt]
  (let [[x y] mouse-pt]
    (if (initial? lasso)
      (assoc (or lasso initial)
        :rect (tuple x y 0 0)
        :last (dissoc lasso :last))
      (let [[lx ly] (:rect lasso)]
        {:last (dissoc lasso :last)
         :rect (tuple
                 lx ly
                 (- x lx)
                 (- y ly))}))))
