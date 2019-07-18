(ns maze.core-test
  (:require [clojure.test :refer :all]
            [maze.core :refer :all]))

(deftest node-test
  (testing "nodes"
    (let [n (->Node [6 7] [])]
      (is (= (.loc n) [6 7]))
      (is (= (.path n) [])))))

(def sample-node-sequence [{:loc [0 1] :path []}
                           {:loc [2 3] :path [[0 1]]}
                           {:loc [3 4] :path [[0 1] [2 3]]}])

(def sample-frontier (->BFSFrontier sample-node-sequence))

(deftest bfs-frontier-test
  (testing "testing BFS frontier"
    (is (= (peek-next-in-frontier sample-frontier) (first sample-node-sequence)))
    (is (= (.nodes (remainder-of-frontier sample-frontier)) (rest sample-node-sequence)))))
