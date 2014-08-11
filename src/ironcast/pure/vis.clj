(ns ironcast.pure.vis
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]]
            [ironcast.util :refer :all]
            [clojure.set :as set]))

(defn visible?
  [world pt]
  (-> world :visible (get pt) boolean))

(defn explored?
  [world pt]
  (-> world :explored (get pt) boolean))

(defn add-visibility
  [world visibility]
  (assoc world :visible visibility
               :explored (set/union (:explored world #{}) visibility)))

(defn los? [world a b]
  (-> (take-while #(transparent-at? world %)
                  (line a b))
      last
      (= b)))

(defn player-visibility
  ([world]
   (player-visibility world 5))
  ([world range]
    (let [transparent-at? #(transparent-at? world %)]
      (->> (for [pl (players world)
                 :let [a (pos world pl)]
                 :when a
                 opaque (-> world :with-flag :opaque)
                 :let [b (pos world opaque)]
                 :when b
                 p (cons b
                         (take-while transparent-at?
                                     (line a b)))
                 :when (<= (manhattan-dist a p) range)]
             p)
           (into #{})))))
