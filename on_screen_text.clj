(ns source2016.on-screen-text
  (:use arcadia.core)
  (:require [clojure.string :as string])
  (:import 
    [UnityEngine GameObject]
    [UnityEngine.UI Text]))

(def max-text-height 30)

(defn left-text [] (.. (object-named "Left Text") (GetComponent Text)))
(defn right-text [] (.. (object-named "Right Text") (GetComponent Text)))

(defn text [txt]
  (.text txt))

(defn text! [txt s]
  (set! (.text txt) s))

(defn clear [txt]
  (text! txt ""))

(defn lines [txt]
  (-> (.text txt)
      (string/split #"\n")
      count))

(defn drop-top [txt n]
  (->> (string/split (.text txt) #"\n")
       (drop n)
       (string/join "\n")
       (text! txt)))

(defn drop-bottom [txt n]
  (->> (string/split (.text txt) #"\n")
       (drop-last n)
       (string/join "\n")
       (text! txt)))

(defn push [txt s]
  (text! txt (str (text txt) "\n" s))
  (drop-top txt (inc (- (lines txt) max-text-height))))

(def colors
  {:boring-beige "#c8b6a2ff"
   :the-gentlemans-teal "#008b87ff"
   :sea-foam "#77a3aaff"
   })

(def default-theme
  [[#"(:[^\s]+)" "#116611ff"]
   [#"\b(\d+)\b" "#991111ff"]
   [#"(;;.*\n)" "#aaaaaaaa"]
   [#"\b(def|defn|map)\b" "#bb0062ff"]
   [#"(\(|\)|\[|\])" "#00000033"]])

(defn highlight-code
  ([s] (highlight-code s default-theme))
  ([s theme]
   (reduce
     (fn [code [pattern color]]
       (string/replace code pattern
                       (str "<color=" color ">$1</color>")))
     s
     theme)))

;;           ;; hide our shame
;;           (if-not (re-find #"Exception" result)
;;             (ost/push (ost/left-text) (ost/highlight-code
;;                                         (str prompt-string code result))))
