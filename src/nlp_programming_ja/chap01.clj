(ns nlp-programming-ja.chap01
  (:require [clojure.string :only [split trim split-lines]]
            [mixi.io :only [slurp-file]]
            [nlp-programming-ja.lib :refer :all]))

(def EOS "</s>")

(defn- count-words [words total-count word-count]
  (if words
    (let [word (first words)]
      (recur (next words)
                   (inc total-count)
                   (if (get word-count word)
                     (update-in word-count [word] inc)
                     (assoc word-count word 1))))
    [total-count word-count]))

(defn count-words-par-line [string]
  (let [words-par-line (map #(conj % EOS) (nlp-programming-ja.lib/split-string-of-each-line string))]
    (reduce #(count-words %2 (first %1) (second %1)) [0 (hash-map)] words-par-line)))

(defn create-model
  ([total-count word-count] (create-model total-count word-count (hash-map)))
  ([total-count word-count model]
    (if word-count
      (let [[w c] (first word-count)]
        (recur total-count (next word-count)
                      (conj model {w (/ (double c) total-count)})))
      model)))

(defn train-unigram [train-filename]
  (let [[total-count word-count] (count-words-par-line (mixi.io/slurp-file train-filename))]
    (create-model total-count word-count)))

(def LAMBDA_UNIGRAM 0.95)
(def LAMBDA_UNK  (- 1.0 LAMBDA_UNIGRAM))
(def V 1000000)

(defn calc-entropy-and-coverage
  ([words model] (calc-entropy-and-coverage words model 0 0 0))
  ([words model word-number unk-number likelihood]
    (if words
     (let [default-p (/ LAMBDA_UNK V)
           probalility (get model (first words))]
       (recur
         (next words)
         model
         (inc word-number)
         (if probalility unk-number (inc unk-number))
         (if probalility
           (- likelihood (nlp-programming-ja.lib/log2 (+ default-p (* probalility LAMBDA_UNIGRAM))))
           (- likelihood (nlp-programming-ja.lib/log2 default-p)))))
        [(/ likelihood word-number) (/ (- word-number unk-number) (double word-number))])))

(defn test-unigram [test-filename model]
   (let [words (mapcat #(conj % EOS) (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file test-filename)))]
     (calc-entropy-and-coverage words model)))

(defn -main [train-filename test-filename]
  (prn (test-unigram test-filename (train-unigram train-filename))
       ))
