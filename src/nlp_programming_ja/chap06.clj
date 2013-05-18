(ns nlp-programming-ja.chap06
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))

(def BOS "<s>")
(def EOS "</s>")

(defn read-lm
  ([filename]
    (let [lines (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file filename))]
      (read-lm lines {})))
  ([lines probs]
    (if lines
      (if (= 2 (count (first lines)))
        (let [[curr prob] (first lines)]
          (recur (next lines) (assoc probs curr (Double. prob))))
        (let [[prev curr prob] (first lines)]
          (recur (next lines) (assoc probs (str prev " " curr) (Double. prob)))))
      probs)))

(defn read-tm
  ([filename]
    (let [lines (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file filename))]
      (read-tm lines [])))
  ([lines probs]
    (if lines
      (let [[e candidate pron prob] (first lines)]
          (recur (next lines) (conj probs {:pron pron :candidate candidate :prob (Double. prob)})))
      (group-by :pron probs))))


(defn forward-step [line lm-probs tm-probs]
  (let [line-count (count line)

        end-step (fn [edge score end]
                   (let [begin-step (fn [begin edge score]
                                      (if (= begin end)
                                        [edge score]
                                        (let [pron (subs line begin end)
                                              my-tm1 (get tm-probs pron)
                                              my-tm (if my-tm1 my-tm1
                                                      (if (= 1 (count pron)) [{:pron pron :candidate pron :prob 0.0001}] []))

                                              tm-step (fn [my-tm edge score]
                                                        (if (seq my-tm)
                                                          (let [tm (first my-tm)
                                                                curr-word (:candidate tm)
                                                                tm-prob   (:prob tm)
                                                                score-step (fn [edge score score-begin]
                                                                             (if (seq score-begin)
                                                                               (let [[prev-word prev-score] (first score-begin)
                                                                                     curr-score (- prev-score (nlp-programming-ja.lib/log2 (* tm-prob (get lm-probs (str prev-word " " curr-word) 0.0001))))]
                                                                                 (if (< curr-score (get-in score [end curr-word] 1e12))
                                                                                   (recur (assoc-in edge [end curr-word] [begin prev-word])
                                                                                          (assoc-in score [end curr-word] curr-score) (next score-begin))
                                                                                   (recur edge score (next score-begin))))
                                                                               [edge score]))

                                                                [next-edge next-score]
                                                                (score-step edge score (vec (get score begin)))]
                                                            (recur (next my-tm) next-edge next-score))
                                                          [edge score]))

                                              [next-edge next-score] (tm-step my-tm edge score)]
                                          (recur (inc begin) next-edge next-score))
                                        ))]
                     (if (= end (inc line-count))
                       [edge score]
                       (let [[next-edge next-score] (begin-step 0 edge score)]
                         (recur next-edge next-score (inc end))))))]
    (end-step [{BOS nil}] [{BOS 0.0}] 1)))


;  (forward-step line lm-probs tm-probs {0 {BOS nil}} {0 {BOS 0.0}} 1 (count line)))
;  ([line lm-probs tm-probs edge score end line-count]
;    (if (= end line-count)
;      [edge score]
;      (let [[next-edge next-score] (forward-step line lm-probs tm-probs edge score 0 end line-count {})]
;        (recur line lm-probs tm-probs next-edge next-score 0 (inc  end) line-count )
;        )
;      )
;    )
;  ([line lm-probs tm-probs edge score begin end line-count my-edges]
;    )
;  )

(defn backward-step
  ([edge score]
    (let [position (dec (count score))
          [min-word _] (reduce #(if (< (second %1) (second %2))  %1 %2) [nil 1e12] (get score position))]
          (backward-step edge min-word position [])))
  ([edge curr-word position words]
    (if (= position 0)
      (clojure.string/join " " (reverse words))
      (let [[prev-position prev-word] (get-in edge [position curr-word])]
        (recur edge prev-word prev-position (conj words curr-word))
      ))))
;  ([edge curr-word prons]
;    (if (not= next-edge [0 BOS])
;      (let [[position tag] next-edge]
;        (recur best-edge (conj tags tag) (get best-edge next-edge)))
;      (clojure.string/join " " (reverse tags))))

(defn -main [lm-filename tm-filename pron-filename]
  (let [lm-probs (read-lm lm-filename)
        tm-probs (read-tm tm-filename)]
    (let [lines (clojure.string/split-lines (mixi.io/slurp-file pron-filename))]
      (doseq [line lines]
        (let [[edge score] (forward-step line lm-probs tm-probs)]
          (prn
            (backward-step edge score))
          )
        ))))
