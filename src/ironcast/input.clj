(ns ironcast.input
  (:require [ironcast.api :as api]
            [ironcast.util :refer :all]
            [clj-tuple :refer [tuple]]
            [ironcast.pure.act :as act]))


(defn handle-cam
  [handled comms]
  (let [cam-fast (comms :cam-fast)
        mult (if cam-fast 2.5 1)
        delta (* @api/delta 500)
        ? #(if (comms %1) (* %2 mult) 0)
        x (+ (? :cam-left (- delta))
             (? :cam-right delta))
        y (+ (? :cam-up delta)
             (? :cam-down (- delta)))]
    (if (not (and (zero? x) (zero? y)))
      (api/shift-cam x y))))

(defn handle-lasso-selection
  [handled comms]
  (when-let [lasso @api/lasso-world]
    (api/unselect-all)
    (api/select-many-in lasso)
    (conj handled :lasso)))

(defn handle-selection
  [handled comms]
  (when (and
          (not (handled :lasso))
          (comms :select)
          @api/selectable-at-mouse?)
    (if (comms :select-many)
      (api/select-at-mouse)
      (api/select-only-at-mouse))
    (conj handled :select)))


(defn handle-player-selection
  [handled comms]
  (when (and
          (not (handled :lasso))
          (comms :select))
    (when-let [p @api/mouse-in-player]
      (if (comms :select-many)
        (api/select p)
        (api/select-only p))
      (api/jump-to-entity p))))

(def key-selection [:player-1 :player-2 :player-3 :player-4 :player-5 :player-6])
(defn handle-key-selection
  [handled comms]
  (-> (for [[player k] (map tuple @api/sorted-players key-selection)
            :when (comms k)]
        (if (and (comms :select-many)
                 (api/selected? player))
          (api/unselect player)
          (do (if (comms :select-many)
                (api/select player)
                (api/select-only player))
              (api/jump-to-entity player))))
      first)
  nil)


(defn handle-default-actions
  [handled comms]
  (when-not (handled :lasso)
    (when (comms :select)
      (let [mc @api/world-cell
            ent (first @api/selected)
            act (api/default-action ent mc)]
        (when act
          (doseq [ent @api/selected
                  :when (api/can-act? act ent mc)]
            (api/act act ent mc)))))))

(def action-comms
  [:action-1 :action-2 :action-3 :action-4 :action-5])

(defn handle-other-action
  [handled comms]
  (let [ent (first @api/selected)
        mc @api/world-cell
        actions (api/other-actions ent mc)]
    (-> (for [[a k] (map tuple
                         actions
                         action-comms)
              :when (and (comms k)
                         (api/can-act? a ent mc))]
          (api/act a ent mc))
        first))
  nil)

;
;(defn handle-cast
;  [handled comms]
;  (when (and @api/casting
;             (comms :select))
;    (cond @api/mouse-in-game? (api/cast-at-mouse)
;          (and
;            @api/casting-single?
;            @api/mouse-in-player) (api/cast-at-player))))

(defn handle-cancel
  [handled comms]
  (when (comms :cancel)
    (cond @api/casting (api/end-cast))
    (conj handled :info)))

(defn only-in-game
  [f]
  (fn [handled comms]
    (when @api/mouse-in-game?
      (f handled comms))))

(defn only-in-mode
  [f mode]
  (fn [handled comms]
    (when (= @api/mode mode)
      (f handled comms))))

(defn only-in-mode
  [f mode]
  (fn [handled comms]
    (when (= @api/mode mode)
      (f handled comms))))

(defn only-realtime-or-player
  [f]
  (fn [handled comms]
    (when (or @api/real?
              @api/player?)
      (f handled comms))))

(defn handler
  [f]
  (fn [handled comms]
    (or (f handled comms) handled)))


(defn handler
  [f]
  (fn [handled comms]
    (or (f handled comms) handled)))

(def handlers

  (map handler
       [(-> handle-lasso-selection
            (only-in-mode :real))

        (-> handle-selection
            only-in-game
            only-realtime-or-player)

        (-> handle-key-selection
            only-in-game
            only-realtime-or-player)

        (-> handle-player-selection
            only-realtime-or-player)

        (-> handle-default-actions
            only-in-game
            only-realtime-or-player)

        (-> handle-other-action
            only-in-game
            only-realtime-or-player)

        handle-cancel
        handle-cam]))

(defn handle-all
  [comms]
  (reduce (fn [handled f] (f handled comms)) #{} handlers))