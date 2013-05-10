(ns nlp-programming-ja.chap02
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split]]
            [mixi.io :only [slurp-file]]))

(def BOS "<s>")
(def EOS "</s>")


(defn count-unigram-and-bigram-for-line [words counts context-counts]
  (if (nnext words)
    (let [f (first  words)
          s (second words)]
      (recur (next words)
             (-> counts
               (nlp-programming-ja.lib/inc-or-set-one (str f " " s))
               (nlp-programming-ja.lib/inc-or-set-one s))
             (-> context-counts
               (nlp-programming-ja.lib/inc-or-set-one f)
               (nlp-programming-ja.lib/inc-or-set-one ""))))
    [counts context-counts]))


(defn count-unigram-and-bigram
  ([string] (count-unigram-and-bigram (map #(concat [BOS] % [EOS] ) (nlp-programming-ja.lib/split-string-of-each-line string)) {} {}))
  ([words-of-each-line counts context-counts]
    (if words-of-each-line
      (let [[c cc] (count-unigram-and-bigram-for-line (first words-of-each-line) counts context-counts)]
        (recur (next words-of-each-line) c cc))
      [counts context-counts])))

(defn create-model
  ([counts context-counts] (create-model counts context-counts {}))
  ([counts context-counts model]
    (if counts
      (let [[ngram c] (first counts)
            context (let [words (clojure.string/split ngram #"\s+")]
                      (if (= 2 (count words))
                        (first words)
                        ""))]
        (recur (next counts) context-counts (conj model {ngram (/ (double c) (get context-counts context))})))
      model)))


(defn train-bigram [train-filename]
  (apply create-model (count-unigram-and-bigram (mixi.io/slurp-file train-filename))))



(def LAMBDA_UNIGRAM 0.95)
(def LAMBDA_BIGRAM  0.95)
(def V 1000000)

(defn calc-entropy [words model likelihood word-counter]
  (if (nnext words)
    (let [f (first words)
          s (second words)
          p1 (+ (* LAMBDA_UNIGRAM (get model f 0)) (/ (- 1 LAMBDA_UNIGRAM) V))
          p2 (+ (* LAMBDA_BIGRAM  (get model (str f " " s) 0)) (* (- 1 LAMBDA_BIGRAM) p1))]
;      (prn p1)
;      (prn p2)
      (recur (next words) model (- likelihood (nlp-programming-ja.lib/log2 p2)) (inc word-counter)))
    [likelihood word-counter]))


(defn test-bigram
  ([test-filename model] (test-bigram (map #(concat [BOS] % [EOS] ) (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file test-filename))) model 0.0 0))
  ([words-of-each-line model likelihood word-counter]
    (if words-of-each-line
      (let [[l wc] (calc-entropy (first words-of-each-line) model likelihood word-counter)]
        (recur (next words-of-each-line) model l wc) )
      (/ likelihood word-counter))))

(defn -main [train-filename test-filename]
  (prn (test-bigram test-filename (train-bigram train-filename))) )
