(ns nlp-programming-ja.chap07
  (:require [nlp-programming-ja.lib :refer :all]
            [clojure.string :only [split split-lines]]
            [mixi.io :only [slurp-file]]))

(def NUM_TOPIC 2)

(defn add-or-set [m k amount]
  (update-in m [k] #(if (nil? %) amount (+ % amount))))

(defn add-counts [word topic docid amount xcounts ycounts]
  [(-> xcounts
     (add-or-set topic amount)
     (add-or-set [word topic] amount))
   (-> ycounts
     (add-or-set docid amount)
     (add-or-set [topic docid] amount))])

(defn initilize ([lines] (initilize lines [] [] {} {}))
  ([lines xcorpus ycorpus xcounts ycounts]
    (if lines
      (let [docid (count xcorpus)
            words (first lines)
            word-step (fn [words topics xcounts ycounts]
                        (if words
                          (let [topic (rand-int NUM_TOPIC)
                                [next-xcounts next-ycounts] (add-counts (first words) topic docid 1 xcounts ycounts)
                                ]
                            (recur (next words) (conj topics topic) next-xcounts next-ycounts))
                          [topics xcounts ycounts]))
            [topics next-xcounts next-ycounts] (word-step words [] xcounts ycounts)
            ]
        (recur (next lines) (conj xcorpus words) (conj ycorpus topics) next-xcounts next-ycounts))
      [xcorpus ycorpus xcounts ycounts]
      )))

(defn sample-one
  ([probs]
    (let [remaining (rand (reduce + 0.0 probs))]
      (sample-one probs remaining 0)))
  ([probs remaining c]
    (let [new-remaining (- remaining (first probs))]
      (if (> 0 new-remaining)
        c
        (recur (next probs) new-remaining (inc c))))
    ))

(defn prob [x y c]
  (/ (+ (get c [x y] 0.0) 0.05) (+ (get c y 0.0) 0.1)))

(defn make-probs [k x y xcounts ycounts probs]
  (if (= k NUM_TOPIC)
    probs
    (let [prob (* (prob x k xcounts) (prob k y ycounts))]
      (recur (inc k) x y xcounts ycounts (conj probs prob)))))

(defn sample-combination [combination xcorpus ycorpus xcounts ycounts ll]
  (if (seq combination)
    (let [[i j] (first combination)
          x (get-in xcorpus [i j])
          y (get-in ycorpus [i j])
          [xcounts ycounts] (add-counts x y i -1 xcounts ycounts)
          probs (make-probs 0 x i xcounts ycounts [])
          new-y (sample-one probs)
          [xcounts ycounts] (add-counts x new-y i 1 xcounts ycounts)
          ]
      (recur (next combination) xcorpus (assoc-in ycorpus [i j] new-y) xcounts ycounts (- ll (nlp-programming-ja.lib/log2 (get probs new-y)))))
    [xcorpus ycorpus xcounts ycounts ll]
    ))

(defn sample [iteration-count xcorpus ycorpus xcounts ycounts]
  (if (zero? iteration-count)
    [xcorpus ycorpus xcounts ycounts]
    (let [combination (for [x (range (count xcorpus)) y (range (count (get xcorpus x)))] [x y])
          [next-xcorpus next-ycorpus next-xcounts next-ycounts ll]
          (sample-combination combination xcorpus ycorpus xcounts ycounts 0)
          ]
      (prn ll)
      (recur (dec iteration-count) next-xcorpus next-ycorpus next-xcounts next-ycounts))))


(defn -main [filename]
  (let [[xcorpus ycorpus xcounts ycounts] (initilize (nlp-programming-ja.lib/split-string-of-each-line (mixi.io/slurp-file filename)))
        [xcorpus ycorpus xcounts ycounts] (sample 100 xcorpus ycorpus xcounts ycounts)]
    (prn xcorpus)
    (prn ycorpus)
    (prn xcounts)
    (prn ycounts)))
