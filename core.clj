(ns source2016.core
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Rigidbody Vector3 GameObject Component]
           ArcadiaState))

;; physics
(defn force! [pos f]
  (doseq [^Rigidbody rb (objects-typed Rigidbody)]
    (let [distance (Vector3/Distance (.. rb position)
                                     pos)
          amp (if (> distance 5)
                     0
                     (/ 1.0 distance))]
      (.AddForceAtPosition rb
                           (v3* f amp)
                           pos
                           ForceMode/Impulse))))

(defn position [v]
  (let [t (type v)]
    (cond 
      (= t Vector3) v
      (= t GameObject) (.. v transform position)
      (isa? t Component) (.. v gameObject transform position))))

;; prototype components api
(defn ensure-component [go c]
  (or (.GetComponent go c)
      (.AddComponent go c)))

(defn state! [go s]
  (let [^ArcadiaState c (ensure-component go ArcadiaState)]
    (set! (.state c) s)))

(defn state [go]
  (let [^ArcadiaState c (ensure-component go ArcadiaState)]
    (.state c)))

(defn swap-state! [go f & args]
  (state! go (apply f (state go) args)))

(defn hook! [go h f]
  (set! (.fn (ensure-component go h)) f))
