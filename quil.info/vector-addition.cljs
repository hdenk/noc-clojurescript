(ns nature-of-code.vectors.vector-subtraction
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def params 
  {:size-x 400 
   :size-y 400
   :background 255
   :frame-rate 30})

(def v1 [100 -100])
(def v2 [-50 -100])

(defn add [& vs]
  "vector addition"
  (vec (apply map + vs)))

(defn draw-arrow [x1 y1 x2 y2 a]
  (q/push-matrix)
  (q/translate x2 y2)
  (q/rotate (q/atan2 (- y2 y1) (- x2 x1)))
  (q/triangle (- (* a  2)) (- a) 0 0 (- (* a  2)) a)
  (q/pop-matrix)
  (q/line x1 y1 x2 y2))  
 
(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)) ; anti aliasing on

(defn draw-sketch []
  (q/background (params :background))
  (q/stroke-weight 2)
  (q/stroke 0)
  (q/no-fill)

  (q/translate (/ (params :size-x) 2.0) (/ (params :size-y) 2.0))
  (q/ellipse 0 0 4 4)
  (q/line 0 0 (first v1) (second v1))
  (let [vr (add v1 v2)]
    (q/line (first v1) (second v1) (first vr) (second vr))
    (draw-arrow 0 0 (first vr) (second vr) 5)))

(q/defsketch vector-subtraction
  :host "host"
  :size [(params :size-x) (params :size-y)]
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
