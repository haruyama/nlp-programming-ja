(ns nlp-programming-ja.chap05
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))

(def BOS "<s>")
(def EOS "</s>")


(defn train-line [wordtags emit transition context previous]
  (if wordtags
    (let [[word tag] (clojure.string/split (first wordtags) #"_" 2)]
      (recur (next wordtags)
             (nlp-programming-ja.lib/inc-or-set-one emit    (str tag " " word))
             (nlp-programming-ja.lib/inc-or-set-one transition (str previous " " tag))
             (nlp-programming-ja.lib/inc-or-set-one context tag)
             tag))
    [emit (nlp-programming-ja.lib/inc-or-set-one transition (str previous " " EOS)) context]))

(defn train
  ([lines emit transition context]
    (if lines
      (let [[e t c] (train-line (first lines) emit transition (nlp-programming-ja.lib/inc-or-set-one context BOS) BOS)]
        (recur (next lines) e t c))
      [emit transition context])))



;(defn -main [model-filename input-filename]
;  (let [[emit transition context]
;        (train (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file train-filename)) {} {} {})]
;    (print
;      (clojure.string/join "\n" (map (fn [[k v]]
;                                       (let [[previous word] (clojure.string/split k #" " 2)]
;                                         (str "T " k " " (/ (double v) (get context previous)))
;                                         )
;                                       ) transition)) "\n")
;    (print
;      (clojure.string/join "\n" (map (fn [[k v]]
;                                       (prn k)
;                                       (let [[tag word] (clojure.string/split k #" " 2)]
;                                         (str "E " tag  " " word " " (/ (double v) (get context tag)))
;                                         )
;                                       ) emit)) "\n")
;    ))

(def POSSIBLE_TAGS [BOS EOS "X" "Y" "Z"])

(def POSSIBLE_TAGS_COMBINATION
  (mapcat (fn [e] (map #(vector e %) POSSIBLE_TAGS)) POSSIBLE_TAGS))

(defn read-model
  ([model-filename] (read-model (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file model-filename)) {} {}))
  ([lines emit transition]
    (if lines
      (let [[t context word prob] (first lines) ]
        (if (= "E" t)
          (recur (next lines) (assoc emit [context word] (Double. prob)) transition)
          (recur (next lines) emit (assoc transition [context word] (Double. prob)))))
      [emit transition])))

(defn forward-step ([words emit transition] (forward-step words emit transition 0 {[0 BOS] nil} {[0 BOS] 0.0}))
  ([words emit transition i best-edge best-score]
    (if (= i (inc (count words)))
      [best-edge best-score]
      (let [[be bs] (forward-step words emit transition i best-edge best-score POSSIBLE_TAGS_COMBINATION)]
        (recur words emit transition (inc i) be bs))))
  ([words emit transition i best-edge best-score tags-combination]
    (if tags-combination
      (let [[p n] (first tags-combination)
            score (if (and (get best-score [i p]) (get transition [p n]))
                    (- (get best-score [i p]) (nlp-programming-ja.lib/log2 (get transition [p n])) (nlp-programming-ja.lib/log2 (get emit [n (get words i)] 1e-10)))
                    1e11)]
        (if (> (get best-score [(inc i) n] 1e12) score)
          (recur words emit transition i (assoc best-edge [(inc i) n] [i p]) (assoc best-score [(inc i) n] score) (next tags-combination))
          (recur words emit transition i best-edge best-score (next tags-combination))))
      [best-edge best-score])))

(defn backward-step ([best-edge l] (backward-step best-edge [] (get best-edge [l EOS])))
  ([best-edge tags next-edge]
    (if (not= next-edge [0 BOS])
      (let [[position tag] next-edge]
        (recur best-edge (conj tags tag) (get best-edge next-edge)))
      (clojure.string/join " " (reverse tags)))))

(defn -main [input-filename model-filename]
  (let [[emit transition] (read-model model-filename)
        lines (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file input-filename))]
    (doseq [words lines]
      (let [[best-edge best-score] (forward-step words emit transition)]
;        (prn best-edge best-score)
        (prn (backward-step best-edge (inc (count words))))
    ))))

