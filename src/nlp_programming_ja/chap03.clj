(ns nlp-programming-ja.chap03
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))

(def PREFIX_UNIGRAM "UNI:")

(defn create-features
  ([line] (create-features (clojure.string/split line #"\s+") {}))
  ([words phi]
    (if words
      (recur (next words)
             (nlp-programming-ja.lib/inc-or-set phi (str PREFIX_UNIGRAM (first words))))
      phi)))


(defn predict-one
  ([w phi] (predict-one w phi 0))
  ([w phi score]
    (if phi
      (let [[n v] (first phi)]
        (recur w (next phi) (+ score (if (get w n) (* (double v) (get w n)) 0.0))))
      (if (>= score 0) 1 -1))))

(defn predict-all [w lines]
  (map #(predict-one w (create-features %)) lines))

(defn update-weights [w phi y]
  (if (not-empty phi)
    (let [[n v] (first phi)
          update (* (double v) y)]
      (recur (if (get w n)
               (update-in w [n] #(+ % update))
               (assoc w n update))
             (next phi)
             y))
    w))

(defn train-1 [w data]
  (if data
    (let [[y string] (first data)
          label (Integer. y)
          phi (create-features string)
          predicted (predict-one w phi)]
      (recur (if (= label predicted) w (update-weights w phi label)) (next data)))
    w))

(defn train [w l data]
  (if (= l 0)
    w
    (recur (train-1 w data) (- l 1) data)))

(defn -main [filename]
  (prn
    (train {} 1 (map #(clojure.string/split % #"\s+" 2) (clojure.string/split-lines (mixi.io/slurp-file filename))))))
