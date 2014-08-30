(ns ironcast.pure.create
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]]
            [ironcast.util :refer :all]))

(defn push
  [world ent opts]
  (cond-> (-> (add-attrs world ent (dissoc opts :pos :flags))
              (add-flags ent (:flags opts)))
          (:type opts) (add-flag ent (:type opts))
          (:pos opts) (put ent (:pos opts))))

(defmulti default-flags identity)
(defmethod default-flags :default [_] nil)

(defmacro def-defaults
  [type flags]
  `(defmethod default-flags ~type [_#] ~flags))

(defmulti push-obj
  (fn [world ent obj] (:type obj)))

(defn- push-obj*
  [world ent obj]
  (-> (push world ent obj)
      (add-flags ent (default-flags (:type obj)))))

(defmethod push-obj :default
  [world ent obj]
  (push-obj* world ent obj))

(def-defaults :wall #{:solid :opaque})

(def-defaults :creature #{:solid})

(def-defaults :door #{:decor})

(defmethod push-obj :door
  [world ent obj]
  (let [world (push-obj* world ent obj)]
    (if (:open? obj)
      (open world ent)
      (close world ent))))

;;from tiled

(defn objectify-terrain
  [terrain]
  [(assoc terrain
     :type (:terrain terrain))])

(defn objectify-decor
  [decor]
  [(assoc decor
     :type :decor)])

(defn objectify-obj
  [obj]
  (for [pt (rect-pts (:rect obj))]
    (assoc (update obj :type read-string)
      :pos pt)))
