(ns maze.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [maze.base :as mb]
            [maze.core :as mc]
            [maze.paral :as mpar]
            [maze.params :as mp]))

(defn setup-trivial-maze
  []
  (mc/read-maze "/home/agold/Prog/maze/trivial4"))

#_(use-fixtures :once setup-trivial-maze )

(defn make-dummy-Node
  "make a dummy node"
  []
  (let [x (rand-int 100)
        y (rand-int 100)
        g (rand-int 50)
        h (rand-int 1000)]
    ;; loc parent g h
    (apply mb/->Node [[x y] [(inc x) y] g h])))

(defn make-sequence-of-Nodes
  [n]
  (repeatedly n make-dummy-Node))

(defn make-test-pq
  "make a PriQ for testing"
  [n]
  (let [queue (mb/priority-queue 1000 mb/node-comp)
        pq (mb/->PriQ queue)]
    (mb/add-nodes! pq (make-sequence-of-Nodes n))
    pq))

#_(deftest test-split-frontier
  "splitting a frontier should produce PriQs of correct size"
  (testing "frontier splitting"
    (let [size 100
          parts 4
          new-frontiers (mc/split-frontier (make-test-pq size) parts)]
      (println "new frontier types" (map type new-frontiers))
      (is (every? #(= maze.core.PriQ %) (map type new-frontiers)))
      (is (every? #(= 25 %) (map mc/countf new-frontiers)))))
  ;; this should return 9 frontiers, 8 with count 12 and 1 with count 4
  (let [size 100
        parts 8
        new-frontiers (mc/split-frontier (make-test-pq size) parts)]
    (is (every? #(<= % 12) (map mc/countf new-frontiers)))))

(deftest test-thread-create
  (testing "thread creation with node"
    (let [start mb/astar-start
          thr (mpar/create-thread-params start)
          pq (:open thr)
          node (mb/get-next! pq)]
      (is (= node start))))
  (testing "thread creation emparty"
    (let [thr (mpar/create-thread-params)]
      (is (mb/deserted? (:open thr))))))


(deftest test-buffer-next-and-put-buffer
  (testing "retrieve from buffer with something in it"
    (let [buffers (mpar/create-buffers mp/nthreads)
          buffer (nth buffers 0)
          in-tuple [[0 1] [0 0] 1 1]
          node (apply mb/->Node in-tuple)]
      (mpar/put-buffer buffer node)
      (let [out-node (mpar/buffer-next buffer)]
        (is (= out-node node))
        (is (nil? (mpar/buffer-next buffer)))))))

(deftest test-get-open
  (testing "get open queue from thread mp")
  (let [tp (mpar/create-thread-params)
        pq (mpar/get-open tp)]
    (is (not (nil? pq)))))

(deftest test-create-expanders
  (testing 
   "creates nthread expanders with start-node in proper receptacle"
    (let [start-node (mb/astar-start)
          expanders (mpar/create-expanders mp/nthreads start-node)
          start-recipient (mpar/compute-recipient start-node)
          nth-tp (nth expanders start-recipient)
          pq (mpar/get-open nth-tp)]
      (is (not (nil? pq)))
      (is (< start-recipient (count expanders)) )
      (is (not (nil? nth-tp)))
      (is (= (mb/countf (mpar/get-open (nth expanders start-recipient))) 1) ))))

(deftest test-trivial
  (testing "trivial maze"
    (setup-trivial-maze)
    (is (=  @mp/start* [1 0]))
    (let [start (mb/start-Node)
          succs (mpar/make-successor-nodes start)]
      (is (= (count succs) 3)))))
