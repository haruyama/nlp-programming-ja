(ns nlp-programming-ja.chap00-test
  (:require [clojure.test :refer :all]
            [nlp-programming-ja.chap00 :refer :all]))

(deftest word-count-test
         (is (= [["aa" 1]]          (nlp-programming-ja.chap00/word-count "aa")))
         (is (= [["A" 1]  ["aa" 1]] (nlp-programming-ja.chap00/word-count "aa A")))
         (is (= [["aa" 2] ["A" 1]]  (nlp-programming-ja.chap00/word-count "aa A aa")))
         (is (= [["A" 1]  ["aa" 1]] (nlp-programming-ja.chap00/word-count "aa\nA"))))
