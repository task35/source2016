(ns source2016.nasser
  (:use arcadia.core
        arcadia.linear
        source2016.core))

(defn camera-control [go]
  ;; lerp to target-position
  (set! (.. go transform position)
        (Vector3/Lerp (.. go transform position)
                      (-> go state :target-position)
                      0.1))
  ;; lerp to look-at
  (let [{:keys [look-at current-look-at]
         :or {current-look-at (v3 0)}}
        (state go)
        lerped-look-at (Vector3/Lerp
                         (position current-look-at)
                         (position look-at)
                         0.1)]
    (swap-state! go assoc :current-look-at lerped-look-at)
    (.. go transform (LookAt lerped-look-at))))

(defn cube-mover [go]
  (.. go transform (Rotate 0.5 0 0) )
  (.. go transform (Translate (v3 (* 1 (Math/Sin (Time/time)))
                                  0
                                  0))))
