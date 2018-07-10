;; ****************************************************************************
;; Vector Arithmetics
;; ****************************************************************************

(ns nature-of-code.math.vector
  "A minimalistic Implementation of basic Vector-Math
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com")

(defn add [& vs]
  "vector addition"
  (vec (apply map + vs)))

(defn multiply [v scalar ]
  "vector multiplication"
  (vec (map * (repeat scalar) v)))

(defn divide [v scalar]
  "vector division"
  (vec (map / v (repeat scalar))))

;; ****************************************************************************
;; Sketch
;; ****************************************************************************

(ns nature-of-code.systems.particlesystem-forces.core
  "Particle-System produces Particles that experience Gravity
  	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size [600 600]
   :background 255
   :frame-rate 30
   :gravity [0.0 0.05]
   :lifespan 255
   :lifespan-dec-rate 2
   :circle-r 16
   :square-l 16
   :particle-color 127
   :particle-outline-thickness 2}) 

(defmacro SIZE-X []
  (first (params :size)))

(defmacro SIZE-Y []
  (second (params :size)))

;;
;; particle
;;

;; TODO clojure.spec verwenden ?)
(defn gen-particle 
  [& {:keys [id shape mass location velocity acceleration lifespan] 
      :or {}}] 
  {:id id :shape shape :mass mass :location location :velocity velocity :acceleration acceleration :lifespan lifespan}) 

(defn apply-force-to-particle [{:keys [mass acceleration] :as particle} force] 
  (let [mf (mv/divide force (float mass))
        next-acceleration (mv/add acceleration mf)]
    (assoc particle :acceleration next-acceleration)))

(defn move-particle [{:keys [mass location velocity acceleration lifespan] :as particle}]
  (let [next-location (mv/add location velocity)
        next-velocity (mv/add velocity acceleration)
        next-acceleration (mv/multiply acceleration 0)
        next-lifespan (- lifespan (params :lifespan-dec-rate))]
    (assoc particle :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))

(defn next-particle [particle force]
  (-> particle 
      (apply-force-to-particle force)
      (move-particle)))

(defn is-expired-particle? [{:keys [lifespan] :as particle}] 
  (< lifespan 0))

(defmulti draw-particle (fn [particle] (:shape particle)))

(defmethod draw-particle :circle [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (params :particle-outline-thickness))
  (q/fill (params :particle-color) lifespan)
  (q/ellipse (first location) (second location) (params :circle-r) (params :circle-r))
  particle)

(defmethod draw-particle :square [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (params :particle-outline-thickness))
  (q/fill (params :particle-color) lifespan)
  (q/rect-mode :center)
  (q/rect (first location) (second location) (params :square-l) (params :square-l)) 
  (q/rect-mode :corner) ; TODO? get current rect-mode from graphic object
  particle)

(defmethod draw-particle :rotating-square [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (params :particle-outline-thickness))
  (q/fill (params :particle-color) lifespan)
  (q/push-matrix)
  (q/translate (first location) (second location))
  (let [theta (q/map-range (first location) 0 (q/width) 0 (* Math/PI 2))]
    (q/rotate theta))
  (q/rect-mode :center)
  (q/rect 0 0 (params :square-l) (params :square-l)) 
  (q/rect-mode :corner) ; TODO? get current rect-mode from graphic object
  (q/pop-matrix)
  particle)

;;
;; particle-system
;;

;; TODO clojure.spec verwenden ?)
(defn gen-particle-system
  [& {:keys [origin gravity particles] 
      :or {}}]
  {:origin origin :gravity gravity :particles particles})

(defn next-particles [{:keys [gravity particles] :as particle-system}]
  (let [next-particles (into [] (map (fn [particle] (next-particle particle gravity))
                                     particles))]
    (assoc particle-system :particles next-particles)))

(defn random-shape []
  (rand-nth [:circle :square :rotating-square]))

(defn add-new-particle [{:keys [origin particles] :as particle-system}]
  (let [next-particles (conj particles 
                         (gen-particle 
                           :id (str "p" (count particles)) 
                           :shape (random-shape)
                           :mass 1.0 
                           :location origin 
                           :velocity [(q/random -1.0 1.0) (q/random -2.0 0.0)] 
                           :acceleration [0 0]
                           :lifespan (params :lifespan)))]
    (assoc particle-system :particles next-particles)))

(defn remove-expired-particles [{:keys [particles] :as particle-system}]
  (let [next-particles (remove is-expired-particle? particles)]
    (assoc particle-system :particles next-particles)))  

(defn next-particle-system [particle-system]
  (-> particle-system 
      (next-particles) 
      (add-new-particle) 
      (remove-expired-particles)))

(defn draw-particle-system [{:keys [particles] :as particle-system}]
  (dorun (map (fn [particle] (draw-particle particle)) particles))
  particle-system)

;;
;; Sketch
;; 

(def particle-system (atom (gen-particle-system 
                             :origin [(/ (SIZE-X) 2) (- (SIZE-Y) (* (SIZE-Y) 0.75))] 
                             :gravity (params :gravity)
                             :particles [])))

(defn setup-sketch []
  ;(js/console.log "setup-sketch")
  (q/frame-rate (params :frame-rate))
  (q/smooth)
  (q/rect-mode :corner))


(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  ; draw particle-system
  (draw-particle-system @particle-system)

  ; update ParticleSystem to next-state
  (swap! particle-system next-particle-system)) 

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch particlesystem-forces 
  :host "host"
  :size (params :size)
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
