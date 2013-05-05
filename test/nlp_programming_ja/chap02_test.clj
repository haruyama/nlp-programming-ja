(ns nlp-programming-ja.chap02-test
  (:require [clojure.test :refer :all])
  (:require [nlp-programming-ja.chap02 :refer :all]))

(deftest count-unigram-and-bigram-test
         (is (= [{"b" 1, "a b" 1, "a" 1, "<s> a" 1} {"a" 1, "" 2, "<s>" 1}] (nlp-programming-ja.chap02/count-unigram-and-bigram "a b")))
         (is (= [{"c" 1, "<s> c" 1, "b" 1, "a b" 1, "a" 1, "<s> a" 1} {"a" 1, "" 3, "<s>" 2}]
                (nlp-programming-ja.chap02/count-unigram-and-bigram "a b\nc")))
         )

(deftest create-model-test
         (let [[counts context-counts] (nlp-programming-ja.chap02/count-unigram-and-bigram "a b c\na b d")]
           (is (= {"<s> a" 1.0, "a" 0.3333333333333333, "a b" 1.0, "b" 0.3333333333333333, "b c" 0.5, "c" 0.16666666666666666, "b d" 0.5, "d" 0.16666666666666666} (nlp-programming-ja.chap02/create-model counts context-counts)))))
