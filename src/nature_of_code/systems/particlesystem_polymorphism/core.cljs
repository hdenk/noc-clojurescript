(ns nature-of-code.systems.particlesystem-polymorphism.core
  "Particle-System produces Particles that experience Gravity
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]))

(def params ^{:doc "DataStructure representing Params to customize the app"}
  {:size [600 400]
   :background 255
   :frame-rate 30
   :gravity [0.0 0.1]
   :lifespan 255
   :lifespan-dec-rate 2
   :circle-r 16
   :square-l 12
   :particle-color 127})

(defn size-x []
  (first (params :size)))

(defn size-y []
  (second (params :size)))

;;
;; Abstractions
;;

(defprotocol Mobile
  (move [this] "enter next motion state for the mobile object"))

(defprotocol Massive
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Expirable
  (expired? [this] "returns true when lifespan is over"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Particle
;;

(defn- particle-next-state [particle]
  (let [next-location (mv/add (:location particle) (:velocity particle))
        next-velocity (mv/add (:velocity particle) (:acceleration particle))
        next-acceleration (mv/multiply (:acceleration particle) (float 0))
        next-lifespan (- (:lifespan particle) (params :lifespan-dec-rate))]
    (assoc particle :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))

(defn- particle-apply-force [particle force]
  (let [mf (mv/divide force (:mass particle))
        next-acceleration (mv/add (:acceleration particle) mf)]
    (assoc particle :acceleration next-acceleration)))

(defn- particle-expired? [particle]
  (< (:lifespan particle) 0))

(defrecord CircularConfetti [id mass location velocity acceleration lifespan]
  Mobile
  (move [this]
    (particle-next-state this))

  Massive
  (apply-force [this force]
    (particle-apply-force this force))

  Expirable
  (expired? [this]
    (particle-expired? this))

  Drawable
  (draw [this]
    (q/stroke 0 (:lifespan this))
    (q/stroke-weight 2)
    (q/fill (params :particle-color) (:lifespan this))
    (q/ellipse (first (:location this)) (second (:location this)) (params :circle-r) (params :circle-r))))

(defrecord SquaredConfetti [id mass location velocity acceleration lifespan]
  Mobile
  (move [this]
    (particle-next-state this))

  Massive
  (apply-force [this force]
    (particle-apply-force this force))

  Expirable
  (expired? [this]
    (particle-expired? this))

  Drawable
  (draw [this]
    (q/fill (params :particle-color) (:lifespan this))
    (q/stroke 0 (:lifespan this))
    (q/stroke-weight 2)
    (q/push-matrix)
    (q/translate (first (:location this)) (second (:location this)))
    (let [theta (q/map-range (first (:location this)) 0 (q/width) 0 (* Math/PI 2))]
      (q/rotate theta))
    (q/rect-mode :center)
    (q/rect 0 0 (params :square-l) (params :square-l))
    (q/pop-matrix)))

(defn gen-particle
  [& {:keys [id mass location velocity acceleration lifespan]
      :or {id "px" mass 0 location [0 0] velocity [0 0] acceleration [0 0] lifespan 0}}]
  (if (> (rand 1) 0.5)
    (CircularConfetti. id mass location velocity acceleration lifespan)
    (SquaredConfetti. id mass location velocity acceleration lifespan)))

;;
;; ParticleSystem
;;

(defn move-particles [particles]
  (map move particles))

(defn add-particle [particles origin particle-count]
  (conj
    particles
    (gen-particle :id (str "p" particle-count) :mass 1.0 :location origin :velocity [(q/random -1.0 1.0) (q/random -2.0 0)] :lifespan (params :lifespan))))

(defn remove-expired [particles]
  (remove expired? particles))

(defrecord ParticleSystem [origin particles particle-count]
  Mobile
  (move [this]
    (let [next-particles
          (-> (:particles this)
              (move-particles)
              (add-particle (:origin this) (:particle-count this))
              (remove-expired))
          next-particle-count (inc (:particle-count this))]
      (assoc this :particles next-particles :particle-count next-particle-count)))

  Massive
  (apply-force [this force]
    (let [next-particles (map #(apply-force % force) (:particles this))]
      (assoc this :particles next-particles)))

  Drawable
  (draw [this]
    (dorun (map #(draw %) (:particles this)))))

;;
;; Sketch
;;

(def particle-system ^{:doc "DataStructure representing a ParticleSystems State"}
  (atom
    (map->ParticleSystem
      {:origin [(/ (size-x) 2) (- (size-y) (* (size-y) 0.75))]
       :particles []
       :particle-count 0})))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255)
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  ; draw Particles
  (draw @particle-system)

  ; update ParticleSystem to next-state
  (let [gravity (params :gravity)]
    (swap!
      particle-system
      #(-> %
           (apply-force gravity)
           (move)))))

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch particlesystem-polymorphism
  :host "particlesystem-polymorphism"
  :size (params :size)
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
