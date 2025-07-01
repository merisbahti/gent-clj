(ns gent-clj.core
  (:require [clojure.string :as str]
            [clojure.test :as test]))

(defn foo "I don't do a whole lot." [x] (println x "Hello, World!"))
(test/deftest meris-test
  (test/testing "some test 2" (test/is (= 1 1)))
  (test/testing "some test 3" (test/is (= 1 3))))
