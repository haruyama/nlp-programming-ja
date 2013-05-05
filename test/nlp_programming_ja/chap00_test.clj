(ns nlp-programming-ja.chap00-test
  (:require [clojure.test :refer :all]
            [nlp-programming-ja.chap00 :refer :all]))

(deftest count-and-sort-words-test
         (is (= [["aa" 1]]          (nlp-programming-ja.chap00/count-and-sort-words "aa")))
         (is (= [["A" 1]  ["aa" 1]] (nlp-programming-ja.chap00/count-and-sort-words "aa A")))
         (is (= [["aa" 2] ["A" 1]]  (nlp-programming-ja.chap00/count-and-sort-words "aa A aa")))
         (is (= [["A" 1]  ["aa" 1]] (nlp-programming-ja.chap00/count-and-sort-words "aa\nA"))))
