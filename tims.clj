(ns source2016.tims
  (:use [source2016 core]
        [arcadia core linear hydrate]
        [clojure pprint repl])
  (:require [clojure.set :as set]
            [arcadia.introspection :as intro]
            [source2016.mesh :as m])
  (:import UpdateHook
           [UnityEngine
            Time
            Color
            PhysicMaterial
            Transform
            BoxCollider
            Rigidbody
            Time
            Collider
            Mesh
            CombineInstance
            PhysicMaterialCombine]
           [UnityEngine
            MeshFilter MeshRenderer Shader
            Material GameObject Component
            Color]
           [UnityEditor
            Selection]
           [UnityEngine
            Quaternion Vector2 Vector3 Transform GameObject Component
            Debug MeshFilter Mesh MeshRenderer Color
            LineRenderer Material Shader
            Gizmos Texture2D Resources Mathf
            Physics Ray RaycastHit
            Input
            Camera
            Application]))


;; ==================================================
;; stuff

(defn kill! [x]
  (let [spec (dehydrate x)]
    (GameObject/Destroy x)
    spec))

;; ============================================================
;; fun macros

(defmacro set-with! [obj [sym & props] & body]
  `(let [obj# ~obj
         ~sym (.. obj# ~@props)]
     (set! (.. obj# ~@props) (do ~@body))))

(defmacro when-set!
  ([obj k v]
   (let [objsym (gensym "obj__")
         access (if (symbol? k)
                  `(. ~objsym ~k)
                  `(.. ~objsym ~@k))]
     `(let [~objsym ~obj]
        (when-let [v# ~v]
          (set! ~access v#)))))
  ([obj k v & kvs]
   (assert (even? (count kvs)))
   (let [objsym (gensym "obj__")]
     `(let [~objsym ~obj]
        ~@(for [[k v] (partition 2 (list* k v kvs))]
            `(when-set! ~objsym ~k ~v))))))

;; yes but you too are gross
(defmacro if-in-let
  ([m [n k] then]
   `(when-let [e# (find ~m ~k)]
      (let [~n (val e#)]
        ~then)))
  ([m [n k] then else]
   `(if-let [e# (find ~m ~k)]
      (let [~n (val e#)]
        ~then)
      ~else)))

;; murp.
(defmacro when-in-set!
  ([obj map path key]
   (let [objsym (gensym "obj__")
         access (if (symbol? path)
                  `(. ~objsym ~path)
                  `(.. ~objsym ~@path))]
     `(let [~objsym ~obj] 
        (if-in-let ~map [v# ~key]
          (set! ~access v#)))))
  ([obj map path key & path-keys]
   (let [objsym (gensym "obj__")
         mapsym (gensym "map__")]
     `(let [~objsym ~obj
            ~mapsym ~map]
        ~@(for [[path key] (partition 2 (list* path key path-keys))]
            `(when-in-set! ~objsym ~mapsym ~path ~key))))))

;; ============================================================
;; physics!

(defn pmc-convert [x]
  (case x
    :average PhysicMaterialCombine/Average
    :minimum PhysicMaterialCombine/Minimum
    :multiply PhysicMaterialCombine/Multiply
    :maximum PhysicMaterialCombine/Maximum
    x))

(defn physic-material ^PhysicMaterial [m]
  (let [^PhysicMaterial pm (PhysicMaterial.)
        m2 (as-> m m
             (if (contains? m :bounce-combine)
               (update m :bounce-combine pmc-convert)
               m)
             (if (contains? m :friction-combine)
               (update m :friction-combine pmc-convert)
               m))]
    (when-in-set! pm m2
      bounceCombine :bounce-combine
      bounciness :bounciness
      frictionCombine :friction-combine
      staticFriction :static-friction
      dynamicFriction :dynamic-friction)
    pm))

;; ============================================================
;; selection
;; this api is probably bullshit

(defn- ensure-set [x]
  (if (set? x) x (set x)))

(def global-selection
  (atom #{}))

(defn sel
  ([] @global-selection)
  ([x] (reset! global-selection #{x}))
  ([x & xs]
   (reset! global-selection (into #{} (conj x xs)))))

(defn sel+ [& xs]
  (swap! global-selection into xs))

(defn sel- [& xs]
  (swap! global-selection
    (fn [s]
      (persistent!
        (reduce disj! (transient s) xs)))))

(defn fsel []
  (first @global-selection))

(defn selall [coll]
  (reset! global-selection (into #{} coll)))

(defn sel? [x]
  (contains? @global-selection x))


;; ============================================================
;; chumpsi

(defn game-object-seq [x]
  (tree-seq
    #(instance? GameObject %)
    (fn [^GameObject y]
      (map (fn [^Transform tr]
             (.gameObject tr))
        (.transform y)))
    x))

;; ============================================================
;; materials

(defn shader-material ^Material [^String name]
  (Material. (Shader/Find name)))

(defn- ensured-mesh-renderer ^MeshRenderer [^GameObject obj]
  (or (.GetComponent obj UnityEngine.MeshRenderer)
    (.AddComponent obj UnityEngine.MeshRenderer)))

(defn color ^Color
  ([^GameObject obj]
   (when-let [^MeshRenderer mr (.GetComponent obj UnityEngine.MeshRenderer)]
     (.. mr material color)))
  ([^GameObject obj, ^Color c]
   (let [^MeshRenderer mr (ensured-mesh-renderer obj)]
     (set! (.. mr material color) c))
   obj))

;; ============================================================
;; raycasting

(comment
  (defn raycast [^Ray ray]
    (when-let [^RaycastHit hit (first (RayCastHelper/raycast ray))]
      {:collider (.collider hit)
       :point (.point hit)
       :transform (.transform hit)}))

  (defn raycast-plane [^Plane plane, ^Ray ray]
    (first (RayCastHelper/raycastPlane plane ray)))

  (defn forward-ray ^Ray [x]
    (let [^Transform t (transform x)]
      (Ray. (.position t) (.forward t))))

  (defn raycast-forward [x]
    (raycast (forward-ray x))))

;; ============================================================
;; meshes

;; (defn combine-meshes ^Mesh
;;   ([meshes] (combine-meshes meshes nil))
;;   ([meshes {:keys [merge-submeshes use-matrices]
;;             :or {merge-submeshes true
;;                  use-matrices false}}]
;;    (let [f (fn [^Mesh m]
;;              (let [^CombineInstance c (CombineInstance.)]
;;                (set! (.mesh c) m)
;;                c))
;;          ^|UnityEngine.CombineInstance[]| cs (->> meshes
;;                                                (map f)
;;                                                (into-array UnityEngine.CombineInstance))
;;          ^Mesh mesh (Mesh.)]
;;      (.CombineMeshes mesh cs
;;        merge-submeshes
;;        use-matrices)
;;      mesh)))


(defn star-points [point-n, outer-r, inner-r]
  (let [angstep (/ (* 2 Mathf/PI) point-n)
        ps1 (->> (range point-n)
              (mapcat
                (fn [i]
                  (let [p1 (v3
                             (Mathf/Cos (* i angstep))
                             (Mathf/Sin (* i angstep))
                             0)
                        p2 (qv* (aa (* Mathf/Rad2Deg (/ angstep 2)) 0 0 1)
                             (v3* p1 (/ inner-r outer-r)))]
                    [p1 p2])))
              (map #(v3* % outer-r 2)))]
    ps1))
