(ns nlp-programming-ja.chap00
  (:require [clojure.string :only [split]]
            [mixi.io :only [slurp-file]]))

(defn- word-count-for-bag-of-words [bag-of-words wc]
  (if bag-of-words
    (let [word (first bag-of-words)]
      (word-count-for-bag-of-words (next bag-of-words)
                                   (if (get wc word)
                                     (update-in wc [word] inc)
                                     (assoc wc word 1))))
    wc))
(defn word-count [string]
  (sort-by second >
           (word-count-for-bag-of-words (clojure.string/split string #"\s+") (hash-map))))

(defn -main [& args]
  (println (word-count (mixi.io/slurp-file (first args)))))
