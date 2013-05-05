(ns nlp-programming-ja.chap01-test
  (:require [clojure.test :refer :all]
            [nlp-programming-ja.chap01 :refer :all]))



(deftest count-words-par-line-test
        (is (= [2 {"a" 1 "</s>" 1}] (nlp-programming-ja.chap01/count-words-par-line "a")))
        (is (= [5 {"a" 1 "b" 1 "c" 1 "</s>" 2}] (nlp-programming-ja.chap01/count-words-par-line "a b\nc")))
         )

(deftest create-model-test
         (is (= {"a" 0.5 "</s>" 0.5} (create-model 2 {"a" 1 "</s>" 1}))))

(deftest calc-entropy-and-coverage-test
         (is (= [10.705476118743713 0.6] (calc-entropy-and-coverage ["a" "b" "c" "d" "</s>"] {"a" 0.33 "b" 0.33 "</s>" 0.33})))
         )
