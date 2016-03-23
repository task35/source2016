(ns source2016.on-screen-text
  (:use arcadia.core)
  (:require [clojure.string :as string])
  (:import 
    [UnityEngine GameObject]
    [UnityEngine.UI Text]))

(defn left-text [] (.. (object-named "Left Text") (GetComponent Text)))
(defn right-text [] (.. (object-named "Right Text") (GetComponent Text)))

(defn text [txt]
  (.text txt))

(defn text! [txt s]
  (set! (.text txt) s))

(defn clear [txt]
  (text! txt ""))

(defn push [txt s]
  (text! txt (str (text txt) "\n" s)))

(push (left-text) "ablssas")
(clear (left-text))

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

(def default-theme
  [[#"(:[^\s]+)" "lime"]
   [#"(\d+)" "orange"]
   [#"(;;.*\n)" "#aaaaaaaa"]
   [#"(defn|map)" "#990000"]
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