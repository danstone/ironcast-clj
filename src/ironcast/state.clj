(ns ironcast.state
  (:import (java.util.concurrent ConcurrentLinkedQueue))
  (:require [ironcast.internals.input :refer [hold press]]))




(def default-commands
  {:cam-left (hold :a)
   :cam-right (hold :d)
   :cam-down (hold :s)
   :cam-up (hold :w)
   :cam-fast (hold :lshift)
   :select (press :left)
   :select-hold (hold :left)
   :select-many (hold :lshift)
   :cancel (press :esc)
   :info (press :right)
   :turn (press :space)
   :player-1 (press :f1)
   :player-2 (press :f2)
   :player-3 (press :f3)
   :player-4 (press :f4)
   :player-5 (press :f5)
   :player-6 (press :f6)
   :action-1 (press :num1)
   :action-2 (press :num2)
   :action-3 (press :num3)
   :action-4 (press :num4)
   :action-5 (press :num5)})


(def default
  {:commands default-commands
   :debug? true
   :move-ms 100
   :vis-ms 250
   :ui-path-ms 60
   :path-ms 30
   :max-fps 60
   :vsync? false
   :fullscreen? false
   :width 1024
   :height 768})

(defonce batch (atom nil))
(defonce cam (atom nil))
(defonce db (atom {}))

(defonce world (ref {}))
(defonce log (ref '()))
(defonce world-text (ref '()))

(defonce input (atom {}))
(defonce commands (atom #{}))
(defonce ui (atom {}))
(defonce ai-loops (atom {}))
(defonce ent-shift (atom {}))
(defonce settings (atom default))
(defonce atlas (atom nil))
(defonce fonts (atom nil))

(defonce on-main-thread (ConcurrentLinkedQueue.))
(defmacro on-main
  [& actions]
  `(.add ^ConcurrentLinkedQueue on-main-thread
         (fn [] ~@actions)))
