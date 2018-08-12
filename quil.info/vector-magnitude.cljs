(ns nature-of-code.vectors.vector-magnitude
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def params 
  {:size-x 400 
   :size-y 300
   :background 255
   :frame-rate 30})

(defn subtract [& vs]
  "vector subtraction"
  (vec (apply map - vs)))

(defn magnitude [v]
  "returns the magnitude of a vector"
  (js/Math.sqrt (reduce + (map #(js/Math.pow % 2) v))))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)) ; anti aliasing on

(defn draw-arrow [x1 y1 x2 y2 a]
  (q/push-matrix)
  (q/translate x2 y2)
  (q/rotate (q/atan2 (- y2 y1) (- x2 x1)))
  (q/triangle (- (* a  2)) (- a) 0 0 (- (* a  2)) a)
  (q/pop-matrix)
  (q/line x1 y1 x2 y2))  

(defn draw-sketch []
  (q/background (params :background))

  (let [mouse [(q/mouse-x) (q/mouse-y)]
        center [(/ (params :size-x) 2.0) (/ (params :size-y) 2.0)]
        s (subtract mouse center)
        m (magnitude s)]
    (q/fill 0)
    (q/no-stroke)
    (q/rect 0 0 m 10)

    (q/translate (/ (params :size-x) 2.0) (/ (params :size-y) 2.0))
    (q/stroke 0)
    (q/stroke-weight 2)
    (q/ellipse 0 0 4 4)
    (draw-arrow 0 0 (first s) (second s) 5)))

(q/defsketch vector-magnitude
  :host "host"
  :size [(params :size-x) (params :size-y)]
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
