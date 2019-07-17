(ns maze.core-test
  (:require [clojure.test :refer :all]
            [maze.core :refer :all]))

(deftest node-test
  (testing "nodes"
    (let [n (->Node [6 7] [])]
      (is (= (.loc n) [6 7]))
      (is (= (.path n) [])))))


