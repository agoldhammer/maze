(ns maze.core-test
  (:require [clojure.test :refer :all]
            [maze.core :refer :all]))

#_(def sample-node-sequence [{:loc [0 1] :path []}
                           {:loc [2 3] :path [[0 1]]}
                           {:loc [3 4] :path [[0 1] [2 3]]}])

(def test-nodes [[[0 1] [] 0 0]
                 [[1 2] [[0 1]] 1 1]
                 [[2 3] [[0 1] [1 2]] 1 3]])

(deftest node-type-test
  (testing "node deftype"
    (let [test-Nodes (mapv #(apply ->Node %) test-nodes)
          results (for [node test-Nodes]
                    (+ (.cost node) (.heuristic node)))]
      (println results)
      (is (= '(0 2 4) results)))))
