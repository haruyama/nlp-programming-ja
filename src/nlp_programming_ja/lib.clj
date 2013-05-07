(ns nlp-programming-ja.lib
  (:require [clojure.string :only [split trim split-lines]]))

(defn split-string-of-each-line [string]
  (map #(clojure.string/split (clojure.string/trim %) #"\s+") (clojure.string/split-lines string)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn inc-or-set [m k]
  (if (get m k)
    (update-in m [k] inc)
    (assoc m k 1)))
