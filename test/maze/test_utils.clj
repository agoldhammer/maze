(ns maze.test-utils
  (:require [maze.base :as mb]
            [maze.core :refer [read-maze]]))

(defn setup-trivial-maze
  []
  (read-maze "trivial4"))

(defn make-dummy-node
  "make a dummy node"
  []
  (let [x (rand-int 100)
        y (rand-int 100)
        g (rand-int 50)
        h (rand-int 1000)]
    ;; loc parent g h
    (apply mb/->Node [[x y] [(inc x) y] g h])))


#_(def test-node (mb/start-node))
#_(def bigger-node (mb/->Node [10 20] test-node 100 200))

(defn make-vec-of-Nodes
  [n]
  (into [] (repeatedly n make-dummy-node)))

(defn make-test-pq
  "make a PriQ for testing"
  [n]
  (let [queue (mb/priority-queue 1000 mb/node-comp)
        pq (mb/->PriQ queue)]
    (mb/add-nodes! pq (make-vec-of-Nodes n))
    pq))
