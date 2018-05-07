;; ****************************************************************************
;; Vector Arithmetics
;; ****************************************************************************

(ns nature-of-code.math.vector
  "A minimalistic Implementation of basic Vector-Math
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com")

(defn add [& vs]
  "vector addition"
  (vec (apply map + vs)))

(defn subtract [& vs]
  "vector subtraction"
  (vec (apply map - vs)))

(defn magnitude [v]
  "returns the magnitude of a vector"
  (js/Math.sqrt (reduce + (map #(js/Math.pow % 2) v))))

(defn normalize [v]
  "vector normalization"
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

(defn multiply [v scalar ]
  "vector multiplication"
  (vec (map * (repeat scalar) v)))

(defn divide [v scalar]
  "vector division"
  (vec (map / v (repeat scalar))))

(defn distance [v1 v2]
  "returns magnitude of v1 - v2 which is a scalar"
  (js/Math.abs (magnitude (subtract v1 v2))))

(defn limit [v upper]
  "returns a vector constrained with an upper magnitude"
  (let [m (magnitude v)]
    (if (> m upper)
      (multiply (normalize v)  upper)
      v)))

(defn with-magnitude [v mag]
  "returns a vector with a certain magnitude"
  (multiply (normalize v) mag ))


;; ****************************************************************************
;; Sketch
;; ****************************************************************************

(ns nature-of-code.vectors.motion101-acceleration.core
  "Demonstration of the basics of motion with vector.
  A 'Mover' object stores location, velocity, and acceleration as vectors
  The motion is controlled by affecting the acceleration (in this case towards the mouse) 
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size-x 800 
   :size-y 200
   :background 255
   :frame-rate 30
   :mover-r 48
   :mover-color 127
   :initial-speed-x 0
   :initial-speed-y 0
   :topspeed 5
   :acceleration-rate 0.2})

(defn do-init-mover [width height m]
  (-> (assoc-in m [:location] [(/ width 2.0) (/ height 2.0)])
      (assoc-in [:velocity] [(params :initial-speed-x) (params :initial-speed-y)])))

(defn init-mover [m-atom width height]
  (swap! m-atom (partial do-init-mover width height)))

(defn do-update-mover [m]
  (let [mouse [(q/mouse-x) (q/mouse-y)]
        acc (mv/subtract mouse (:location m))
        acc (mv/with-magnitude acc (params :acceleration-rate))]
    (-> (update-in m [:velocity] #(mv/add % acc))
        (update-in [:location] #(mv/add % (:velocity m)))
        (update-in [:velocity] #(mv/limit % (:top-speed m))))))

(defn update-mover [m-atom]
  (swap! m-atom do-update-mover))

(defn draw-mover [m]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill (params :mover-color) 100)
  (apply #(q/ellipse %1 %2 (params :mover-r) (params :mover-r)) (:location m)))

(def mover (atom {:location []
                  :velocity []
                  :top-speed (params :topspeed)}))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth) ; anti aliasing on
  (init-mover mover (params :size-x) (params :size-y)))

(defn draw-sketch []
  (q/background (params :background))
  (update-mover mover)
  (draw-mover @mover))

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch motion101-acceleration
  :host "host"
  :size [(params :size-x) (params :size-y)]
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
