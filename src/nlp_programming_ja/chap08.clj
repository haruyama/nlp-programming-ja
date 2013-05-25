(ns nlp-programming-ja.chap08
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))

(defrecord Grammer [nonterm preterm])
(defrecord SuperGraph [best-edge best-score])

(defn read-grammer
  ([filename] (read-grammer
                (clojure.string/split-lines (mixi.io/slurp-file filename))
                []
                {}))
  ([lines nonterm preterm]
    (if lines
      (let [line (first lines)
            [lhs rhs prob] (clojure.string/split line #"\t")
            rhs-symbols    (clojure.string/split rhs #" ")]
        (if (= 2 (count rhs-symbols))
          (recur (next lines) (conj nonterm [lhs (get rhs-symbols 0) (get rhs-symbols 1) (nlp-programming-ja.lib/log2 (Double. prob))])
                 preterm)
          (recur (next lines) nonterm
                 (update-in preterm [rhs] #(let [element [lhs (nlp-programming-ja.lib/log2 (Double. prob))]]
                                             (if ( nil? %) [element]
                                               (conj % element)))))))
      (Grammer. nonterm preterm))))

(defn update-best-score
  ([index word preterm best-score] (update-best-score index (get preterm word) best-score))
  ([index probs best-score]
    (if probs
      (let [[lhs log-prob] (first probs)]
        (recur index (next probs) (assoc best-score [lhs index (inc index)] log-prob)))
      best-score)))

(defn initilize-super-graph
  ([words grammer] (initilize-super-graph 0 words (:preterm grammer) {} ))
  ([index words preterm best-score]
    (if words
      (recur (inc index ) (next words) preterm (update-best-score index (first words) preterm best-score))
      (SuperGraph. {}  best-score))))


(defn- parse-combination [combination nonterm super-graph]
  (if combination
    (let [[i j k] (first combination)]
      (recur (next combination) nonterm
             (loop [nonterm nonterm super-graph super-graph]
               (if nonterm
                 (let [[sym lsym rsym log-prob] (first nonterm)
                       best-score (:best-score super-graph)
                       best-edge  (:best-edge super-graph)]
                   (if (and (find best-score [lsym i k]) (find best-score [rsym k j]))
                     (let [my-lp (+ (get best-score [lsym i k]) (get best-score  [rsym k j]) log-prob)]
                       (if (> my-lp (get best-score [sym i j] Double/NEGATIVE_INFINITY))
                         (recur (next nonterm) (SuperGraph. (assoc best-edge [sym i j] [[lsym i k] [rsym k j]]) (assoc best-score [sym i j] my-lp)))
                         (recur (next nonterm) super-graph)))
                     (recur (next nonterm) super-graph)))
                 super-graph))))
    super-graph))


(defn parse [words grammer super-graph]
  (let [combination (for [j (range 2 (inc (count words))) i (range (- j 2) -1 -1) k (range (inc i) j)] [i j k])]
    (parse-combination combination (:nonterm grammer) super-graph)))

(defn print-tree
  ([words super-graph] (print-tree words (:best-edge super-graph) ["S" 0 (count words)]))
  ([words best-edge edge]
    (let [[sym i _] edge]
      (if (find best-edge edge)
        [sym
         (print-tree words best-edge (get-in best-edge [edge 0]))
         (print-tree words best-edge (get-in best-edge [edge 1]))]
        [sym (get words i)]))))


(defn -main [grammer-filename input-filename]
  (let [grammer (read-grammer grammer-filename)
        lines   (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file input-filename))]
    (doseq [words lines]
      (prn
        (print-tree words (parse words grammer (initilize-super-graph words grammer)))))))
