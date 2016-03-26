(ns source2016.mesh
  (:use arcadia.core arcadia.linear)
  (:import [UnityEngine Mesh Vector3]
           [LibTessDotNet Tess ContourVertex]
           [UnityEngine MeshFilter MeshRenderer
            Debug GameObject Component
            Mesh Vector3 Vector2
            Mathf Plane
            CombineInstance]))

;; ============================================================
;; utils
;; ============================================================

(defn max-by [keyfn coll]
  (when-let [x (first coll)]
    (first
      (reduce
        (fn [[x kx :as prev] y]
          (let [ky (keyfn y)]
            (if (< kx ky)
              [y ky]
              prev)))
        [x (keyfn x)]
        (rest coll)))))

(defn min-by [keyfn coll]
  (when-let [x (first coll)]
    (first
      (reduce
        (fn [[x kx :as prev] y]
          (let [ky (keyfn y)]
            (if (> kx ky)
              [y ky]
              prev)))
        [x (keyfn x)]
        (rest coll)))))

(defn delete-duplicates
  "Returns a lazy sequence removing duplicates in coll.
  Returns a transducer when no collection is provided."
  ([]
   (fn [rf]
     (let [pv (volatile! #{})] ;; TODO: use faster imperative datastructure
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv]
            (if (prior input)
              result
              (do (vreset! pv (conj prior input))
                  (rf result input)))))))))
  ([coll] (sequence (delete-duplicates) coll)))

(defn index-map [coll]
  (zipmap coll (range)))

(defn wrap-1 [coll]
  (concat coll (take 1 coll)))

;; ============================================================
;; v3 
;; ============================================================

(defn v3-max-min [v3coll]
  [(v3
     (apply max
       (map (fn [^Vector3 v] (.x v))
         v3coll))
     (apply max
       (map (fn [^Vector3 v] (.y v))
         v3coll))
     (apply max
       (map (fn [^Vector3 v] (.z v))
         v3coll)))
   (v3
     (apply min
       (map (fn [^Vector3 v] (.x v))
         v3coll))
     (apply min
       (map (fn [^Vector3 v] (.y v))
         v3coll))
     (apply min
       (map (fn [^Vector3 v] (.z v))
         v3coll)))])

;; ============================================================
;; backface
;; ============================================================

;; based on http://answers.unity3d.com/questions/280741/how-make-visible-the-back-face-of-a-mesh.html
(defn backfaced-mesh ^Mesh [^Mesh mesh]
  (let [vs (.vertices mesh)
        szv (count vs)
        uvs (.uv mesh)
        szu (count uvs)
        norms (.normals mesh)
        szn (count norms)
        tris (.triangles mesh)
        szt (count tris) 
        ^|UnityEngine.Vector3[]| new-vs (make-array Vector3 (* szv 2)) 
        ^|UnityEngine.Vector2[]| new-uvs (make-array Vector2 (* szu 2))
        ^|UnityEngine.Vector3[]| new-norms (make-array Vector3 (* szn 2))
        ^|System.Int32[]| new-tris (int-array (* szt 2))]
    (dotimes [j szv]
      (let [^Vector3 v (aget vs j)]
        (aset new-vs j v)
        (aset new-vs (+ j szv) v))
      (let [^Vector2 uv (aget uvs j)]
        (aset new-uvs j uv)
        (aset new-uvs (+ j szv) uv))
      (let [n (aget norms j)]
        (aset new-norms j n)
        (aset new-norms (+ j szv) (v3- n))))
    (loop [i (int 0)]
      (when (< i szt)
        (aset new-tris i       (aget tris i))
        (aset new-tris (+ 1 i) (aget tris (+ 1 i)))
        (aset new-tris (+ 2 i) (aget tris (+ 2 i)))
        (let [j (+ i szt)]
          (aset new-tris j       (+ (aget tris i)       szv))
          (aset new-tris (+ 1 j) (+ (aget tris (+ 2 i)) szv))
          (aset new-tris (+ 2 j) (+ (aget tris (+ 1 i)) szv)))
        (recur (+ i 3))))
    (let [mesh2 (Mesh.)]
      (set! (.vertices mesh2) new-vs)
      (set! (.uv mesh2) new-uvs)
      (set! (.normals mesh2) new-norms)
      (set! (.triangles mesh2) new-tris) ;; evidently important to do this last
      mesh2)))

;; ============================================================
;; LibTess interop
;; ============================================================

(defn tessv-to-unityv ^UnityEngine.Vector3 [^LibTessDotNet.Vec3 v]
  (v3 (.X v) (.Y v) (.Z v)))

(defn unityv-to-tessv ^LibTessDotNet.Vec3 [^UnityEngine.Vector3 v]
  (LibTessDotNet.Vec3/qwikVec3 (.x v) (.y v) (.z v)))

(defn unityv-to-contourv ^ContourVertex [^Vector3 v]
  (let [^ContourVertex cv (ContourVertex.)]
    (set! (.Position cv) (unityv-to-tessv v))
    cv))

(defn triangulate-tess ^Tess [v3s] 
  (let [^|LibTessDotNet.ContourVertex[]| contour (->> v3s
                                                   (map unityv-to-contourv)
                                                   (into-array LibTessDotNet.ContourVertex))]
    
    (let [^Tess tess (Tess.)]
      (.AddContour tess contour) ;; can add some winding specification thing as extra arg here
      (.Tessellate tess
        LibTessDotNet.WindingRule/EvenOdd,
        LibTessDotNet.ElementType/Polygons,
        3)
      tess)))

;; ============================================================
;; polygon triangulation
;; ============================================================

(defn triangulate ^Mesh [v3s]
  (let [^Tess tess (triangulate-tess v3s)
        ^|UnityEngine.Vector3[]| vs (->> (.Vertices tess)
                                      (map (fn [^ContourVertex v]
                                             (tessv-to-unityv (.Position v))))
                                      (into-array Vector3))
        ^|UnityEngine.Vector2[]| uvs (into-array |UnityEngine.Vector2|
                                       (take (count vs) ;; stupid for now
                                         (cycle [(v2 0 0) (v2 0 1) (v2 1 0)])))
        ^Mesh mesh (Mesh.)]
    (set! (.vertices mesh) vs)
    (set! (.uv mesh) uvs)
    (set! (.triangles mesh) (.Elements tess))
    (.RecalculateNormals mesh)
    mesh))

;; ============================================================
;; extrude
;; ============================================================

(defn tri-quad [p1, p2, p3, p4]
  [p1 p2 p4 p4 p2 p3])

(defn extrude-edge [^Vector3 p1, ^Vector3 p2, ^Vector3 depth-vec]
  (tri-quad p1, (v3+ p1 depth-vec), (v3+ p2 depth-vec), p2))

(defn extrude [v3s, depth-vec]
  (->> v3s
    (partition 2 1)
    (mapcat (fn [[p1 p2]]
              (extrude-edge p1 p2 depth-vec)))))

;; ============================================================
;; mesh
;; ============================================================

;; could stand to be optimized but whatever

(defn combine-meshes ^Mesh [meshes]
  (let [f (fn [^Mesh m]
            (let [^CombineInstance c (CombineInstance.)]
              (set! (.mesh c) m)
              c))
        ^|UnityEngine.CombineInstance[]| cs (->> meshes
                                              (map f)
                                              (into-array UnityEngine.CombineInstance))
        ^Mesh mesh (Mesh.)]
    (.CombineMeshes mesh cs true false)
    mesh))

(def fm-log
  (atom []))

(defn facet-mesh
  ":vertices is a collection of vertices in triangle-winding
  order, :uvs is a map from vertices to uv coordinates. Equal vertices
  will be stored once per facet (no doubling up), corresponding to a
  smooth surface under Unity's rendering convention."
  ^Mesh [{:keys [vertices uvs :as input]}]
  (swap! fm-log conj input)
  (assert (map? uvs))
  (let [vset (set vertices)
        ^|UnityEngine.Vector3[]| vs-ar (into-array UnityEngine.Vector3 vset)
        vinxmap (->> vs-ar
                  (map-indexed
                    (fn [i v] [v i]))
                  (into {}))
        ^|UnityEngine.Vector2[]| uvs-ar (->> vs-ar
                                          (map (fnil uvs (v2 0))) ;; why not
                                          (into-array UnityEngine.Vector2))
        ^|System.Int32[]| tris (->> vertices
                                 (map vinxmap)
                                 (into-array System.Int32))
        ^Mesh mesh (Mesh.)]
    (set! (.vertices mesh) vs-ar)
    (set! (.uv mesh) uvs-ar)
    (set! (.triangles mesh) tris)
    mesh))

(def mesh-log
  (atom []))

(defn mesh
  "Takes a collection of facet maps with keys :vertices and :uvs, makes a mesh. Vertices within facets will be assumed to be smoothly connected (ie shared)."
  ^Mesh [facet-maps]
  (swap! mesh-log conj facet-maps)
  (->> facet-maps (map facet-mesh) combine-meshes))

;; ============================================================
;; polygon extrude
;; ============================================================

(defn clone-mesh ^Mesh [^Mesh m]
  (let [^Mesh m2 (Mesh.)]
    (set! (.vertices m2) (.vertices m))
    (set! (.triangles m2) (.triangles m))
    (set! (.uv m2) (.uv m))
    (set! (.normals m2) (.normals m))
    m2))

(defn translate-mesh ^Mesh [^Mesh m, ^Vector3 v]
  (let [^|UnityEngine.Vector3[]| vs (.vertices m)
        ^Mesh m2 (clone-mesh m)]
    (dotimes [i (count vs)]
      (aset vs i (v3+ (aget vs i) v)))
    (set! (.vertices m2) vs)
    m2))

(defn reverse-mesh ^Mesh [^Mesh m]
  (let [^Mesh m2 (clone-mesh m)
        ^|UnityEngine.Vector3[]| nrms (.normals m2)
        ^|System.Int32[]| tris (.triangles m2)]
    (dotimes [i (count nrms)]
      (aset nrms i (v3- (aget nrms i))))
    (System.Array/Reverse tris)
    (set! (.triangles m2) tris)
    (set! (.normals m2) nrms)
    m2))

(defn polygon-extrude ^Mesh [extrusion-vector v3s]
  (let [^Mesh t1 (triangulate v3s)
        ^Mesh t2 (translate-mesh
                   (reverse-mesh t1)
                   extrusion-vector)
        extvs (vec (extrude v3s extrusion-vector))
        ^Mesh ext (mesh
                    (for [vs (partition 6 extvs)
                          :let [[p1 p2 p4 _ _ p3] vs ;; see tri-quad
                                ]]
                      {:vertices vs
                       :uvs {p1 (v2 1 0)
                             p2 (v2 1 1)
                             p3 (v2 0 1)
                             p4 (v2 0 0)}}))
        ^Mesh m (combine-meshes [t1 ext t2])]
    (.RecalculateNormals m)
    m))
