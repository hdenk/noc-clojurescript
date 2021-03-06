(ns nature-of-code.introduction.random-distribution.core
  "visualize distribution of random number
	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def params 
  {:size [800 200]
   :background 0
   :frame-rate 30})

(def random-counts 
  (atom (vec (repeat 5 (float 0)))))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate)))

(defn draw-sketch []
  (q/background (params :background))
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)

  (let [rc-count (count @random-counts)]
    ; Pick a random number and increase the count
    (let [index (int (q/random rc-count))]
      (swap! random-counts update-in [index] inc))

    ; Draw a rectangle to graph results
    (let [w (/ (q/width) rc-count)]
      (dotimes [x rc-count] 
        (let [r-count (get @random-counts x)]
          (q/rect (* x w) (- (q/height) r-count) (dec w) r-count)))))
  random-counts) 

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch random-distribution
  :host "host"
  :size (params :size)
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
