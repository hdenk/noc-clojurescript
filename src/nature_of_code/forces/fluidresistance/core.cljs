(ns nature-of-code.forces.fluidresistance.core
  "Demonstration of multiple force acting on bodies (Mover class)
  Bodies experience gravity continuously
  Bodies experience fluid resistance when in water
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as qc :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]))

(def params
  {:size-x 600
   :size-y 400
   :background 255
   :frame-rate 30
   :mover-count 5
   :mass-classes 3
   :r-factor 16
   :re-bouncing-factor -0.6
   :initial-speed-x 0
   :initial-speed-y 0
   :initial-acceleration-x 0
   :initial-acceleration-y 0
   :drag-coefficient 0.2
   :mover-color 127
   :fluid-color 211})

;;;
;;; Fluid
;;;

(defrecord Fluid [id x y width height color drag-coefficient])

(defn contains-mover?
  "takes a fluid and a mover and returns true, if the mover is inside the fluid"
  [{:keys [x y width height]} {:keys [location]}]
  (let [[mover-x mover-y] location]
    (if (and
          (>= mover-x x) (<= mover-x (+ x width))
          (>= mover-y y) (<= mover-y (+ y height)))
      true
      false)))

(defn drag-force
  "takes a fluid and a mover and returns drag-force"
  [{:keys [drag-coefficient] :as fluid} {:keys [velocity] :as mover}]
  (if (contains-mover? fluid mover)
    (let [speed (mv/magnitude velocity)
          drag-magnitude (* drag-coefficient speed speed)
          drag-force (mv/multiply velocity (float -1))]
      (-> (mv/normalize drag-force)
          (mv/multiply (float drag-magnitude))))
    [0 0]))

(defn draw-fluid
  [{:keys [x y width height color] :as fluid}]
  (qc/no-stroke)
  (qc/fill color)
  (qc/rect x y width height)
  fluid)

(defn make-fluid []
  (map->Fluid
    {:id "fluid1"
     :x 0 :y (* (params :size-y) 0.75) :width (params :size-x) :height (params :size-y)
     :color (params :fluid-color) :drag-coefficient (params :drag-coefficient)}))

;;;
;;; Mover
;;;

(defrecord Mover [id mass location velocity acceleration color])

(defn apply-force [{:keys [acceleration mass] :as mover} force]
  "takes a mover and a force, applies the force and returns a mover with changed acceleration"
  ; Newton's 2nd law: F = M * A
  ; or A = F / M"
  (let [f (mv/divide force (float mass))
        next-acceleration (mv/add acceleration f)]
    (assoc mover :acceleration next-acceleration)))

(defn apply-gravity [mover]
  ; Gravity is scaled by mass here!
  (let [gravity [0 (* 0.1 (:mass mover))]]
    (apply-force mover gravity)))

(defn update-motion-state
  "takes a mover and force returns a mover with updated motion-state"
  [{:keys [location velocity acceleration] :as mover}]
  (let [next-location (mv/add location velocity)
        next-velocity (mv/add velocity acceleration)
        next-acceleration (mv/multiply acceleration (float 0))]
    (assoc mover :location next-location :velocity next-velocity :acceleration next-acceleration)))

(defn check-edges [{:keys [location velocity mass] :as mover}]
  (let [[x y] location
        rh (/ (* (params :r-factor) mass) 2)
        y-max (- (qc/height) rh)]
    (if (> y y-max)
      (assoc mover :location [x y-max] :velocity (mv/multiply velocity (float (params :re-bouncing-factor))))
      mover)))

(defn update-mover
  "takes a mover and force returns a mover with updated motion-state and applied force"
  [mover fluid]
  (let [drag-force (drag-force fluid mover)]
    (-> (apply-gravity mover)
        (apply-force drag-force)
        (update-motion-state)
        (check-edges))))

(defn draw-mover
  [{:keys [location mass color] :as mover}]
  (qc/stroke 0)
  (qc/stroke-weight 2)
  (qc/fill color, 200)
  (let [[x y] location]
    (qc/ellipse x y (* mass (params :r-factor)) (* mass (params :r-factor))))
  mover)

(defn make-movers []
  (map
    (fn [id]
      (map->Mover
        {:id (str "mover" id)
         :mass (inc (rand-int (params :mass-classes)))
         :location [(rand-int (params :size-x)) (/ (rand-int (params :size-y)) 2)]
         :velocity [(params :initial-speed-x) (params :initial-speed-y)]
         :acceleration [(params :initial-acceleration-x) (params :initial-acceleration-y)]
         :color (params :mover-color)}))
    (range (params :mover-count))))

;;;
;;; Main
;;;

(def sketch-model
  (atom
    { :fluid nil
     :movers nil}))

(defn init-sketch-model [m-atom]
  (swap!
    m-atom
    (fn [m]
      (-> (assoc-in m [:fluid] (make-fluid))
          (assoc-in [:movers] (make-movers))))))

(defn update-movers [fluid movers]
  (map #(update-mover % fluid) movers))

(defn setup-sketch []
  (qc/frame-rate (params :frame-rate))
  (qc/smooth) ; anti aliasing on
  (init-sketch-model sketch-model))

(defn draw-sketch []
  ; draw Background
  (qc/background (params :background))

  ; draw fluid
  (draw-fluid (@sketch-model :fluid))

  ; draw movers
  (dorun (map #(draw-mover %) (@sketch-model :movers)))

  ; draw hint(s)
  (qc/fill 0)
  (qc/text "click mouse to reset" 10 30)

  ; update sketch-model to next state
  (swap!
    sketch-model
    #(update-in
       %
       [:movers]
       (partial update-movers (@sketch-model :fluid)))))

(defn mouse-pressed []
  (swap!
    sketch-model
    #(update-in
       %
       [:movers]
       (constantly (make-movers)))))

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(qc/defsketch fluidresistance
  :host "fluidresistance"
  :size [(params :size-x) (params :size-y)]
  :setup setup-sketch
  :draw draw-sketch
  :mouse-pressed mouse-pressed
  :middleware [m/fun-mode])
