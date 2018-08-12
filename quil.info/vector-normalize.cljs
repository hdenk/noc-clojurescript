(ns nature-of-code.vectors.vector-normalize
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def params 
  {:size-x 400 
   :size-y 300
   :background 255
   :frame-rate 30
   :arrow-length 150})

(defn subtract [& vs]
  "vector subtraction"
  (vec (apply map - vs)))

(defn multiply [v scalar ]
  "vector multiplication"
  (vec (map * (repeat scalar) v)))

(defn magnitude [v]
  "returns the magnitude of a vector"
  (js/Math.sqrt (reduce + (map #(js/Math.pow % 2) v))))

(defn normalize [v]
  "vector normalization"
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

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
  (q/background 255)
  
  (let [mouse [(q/mouse-x) (q/mouse-y)]
        center [(/ (params :size-x) 2.0) (/ (params :size-y) 2.0)]
        s (subtract mouse center)
        n (normalize s)
        m (multiply n (params :arrow-length))]
    (q/translate (/ (params :size-x) 2.0) (/ (params :size-y) 2.0))
    (q/stroke 0)
    (q/stroke-weight 2)
    (draw-arrow 0 0 (first m) (second m) 5)))

(q/defsketch vector-normalize
  :host "host"
  :size [(params :size-x) (params :size-y)]
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
