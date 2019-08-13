(ns maze.core-test
  (:require [clojure.test :refer [deftest is]]
            [maze.core :as m :refer :all]))


(defn make-dummy-Node
  "make a dummy node"
  []
  (let [x (rand-int 100)
        y (rand-int 100)
        g (rand-int 50)
        h (rand-int 1000)]
    ;; loc parent g h
    (apply m/->Node [[x y] [(dec x) y] g h])))

(defn make-sequence-of-Nodes
  [n]
  (repeatedly n make-dummy-Node))

(defn make-test-pq
  "make a PriQ for testing"
  [n]
  (let [queue (m/priority-queue 1000 m/node-comp)
        pq (m/->PriQ queue)]
    (m/add-nodes pq (make-sequence-of-Nodes n))
    pq))

(deftest test-split-frontier
  "splitting a frontier should produce PriQs of correct size"
  (let [size 100
        parts 4
        new-frontiers (m/split-frontier (make-test-pq size) parts)]
    (is (every? #(= maze.core.PriQ %) (map type new-frontiers)))
    (is (every? #(= 25 %) (map m/countf new-frontiers)))))



