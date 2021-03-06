(ns nature-of-code.introduction.random-walk.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def params
  {:size [600 400]
   :background 0
   :frame-rate 30
   :rect-width 20
   :rect-height 20})

(def walker
  (atom
    {:x (/ (first (params :size)) 2)
     :y (/ (second (params :size)) 2)}))

(defn walk []
  "Randomly move up, down, left, right, or stay in one place"
  (let [dx (q/random -2 2)
        dy (q/random -2 2)
        x (+ (:x @walker) dx)
        y (+ (:y @walker) dy)]
    (swap! walker assoc
           :x (q/constrain x 0 (dec (q/width)))
           :y (q/constrain y 0 (dec (q/height)))))
  walker)

(defn setup []
  (q/frame-rate (params :frame-rate)))

(defn draw []
  (q/background (params :background))
  (q/stroke 0)
  (q/fill 175)
  (q/rect-mode :center)
  (q/rect (:x @walker), (:y @walker), (params :rect-width), (params :rect-height)))

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch random-walk
  :host "random-walk"
  :size (params :size)
  :setup setup
  :update walk
  :draw draw
  :middleware [m/fun-mode])
