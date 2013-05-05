(ns nlp-programming-ja.chap00
  (:require [clojure.string :only [split]]
            [mixi.io :only [slurp-file]]))

(defn- count-words [bag-of-words wc]
  (if bag-of-words
    (let [word (first bag-of-words)]
      (recur (next bag-of-words)
             (if (get wc word)
               (update-in wc [word] inc)
               (assoc wc word 1))))
    wc))

(defn count-and-sort-words [string]
  (sort-by second >
           (count-words (clojure.string/split string #"\s+") (hash-map))))

(defn -main [& args]
  (println (count-and-sort-words (mixi.io/slurp-file (first args)))))
