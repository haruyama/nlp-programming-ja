(ns nlp-programming-ja.lib-test
  (:require [clojure.test :refer :all])
  (:require [nlp-programming-ja.lib :refer :all]))


(deftest split-string-of-each-line-test
         (is (= [["hoge" "guha"] ["nyo" "tyo"]] (nlp-programming-ja.lib/split-string-of-each-line "hoge guha\n nyo  tyo")))
         )
