(ns nlp-programming-ja.chap04
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))


(defn forward-step ([line probs] (forward-step line probs 1 [nil] [0]))
  ([line probs word-end best-edge best-score]
    (if (= word-end (inc (count line)))
      [best-edge best-score]
      (let [[be bs] (forward-step line probs 0 word-end best-edge (assoc best-score word-end 1e11))]
        (recur line probs (inc word-end) be bs))))
  ([line probs word-begin word-end best-edge best-score]
    (if (= word-begin word-end)
      [best-edge best-score]
      (let [word (subs line word-begin word-end)
            logprob (nlp-programming-ja.lib/log2 (get probs word 1e-5)) ;scamped work
            score (- (get best-score word-begin) logprob)]
        (if (< score (get best-score word-end))
          (recur line probs (inc word-begin) word-end (assoc best-edge word-end [word-begin word-end]) (assoc best-score word-end score))
          (recur line probs (inc word-begin) word-end best-edge best-score))))))

(defn backward-step ([line best-edge ] (backward-step line best-edge [] (get best-edge (- (count best-edge) 1)) ))
  ([line best-edge words next-edge]
    (if next-edge
      (let [word (subs line (first next-edge) (second next-edge))]
        (recur line best-edge (conj words word) (get best-edge (first next-edge))))
      (clojure.string/join " " (reverse words)))))


(defn word-split [lines probs results]
  (if lines
    (let [line (first lines)
          [best-edge _] (forward-step line probs)]
      (recur (next lines) probs (conj results (backward-step line best-edge))))
    results))

(defn read-model [model-filename]
  (reduce #(assoc %1 (first %2) (Double. (second %2))) {} (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file model-filename))))

(defn -main [input-filename model-filename]
  (word-split (clojure.string/split-lines (mixi.io/slurp-file input-filename)) (read-model model-filename) []))
