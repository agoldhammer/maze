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

(defn make-vec-of-Nodes
  [n]
  (into [] (repeatedly n make-dummy-Node)))

(defn make-test-pq
  "make a PriQ for testing"
  [n]
  (let [queue (mb/priority-queue 1000 mb/node-comp)
        pq (mb/->PriQ queue)]
    (mb/add-nodes! pq (make-vec-of-Nodes n))
    pq))

(defn test-algo
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [closed open thread-num]
  (let [nodes (mpar/buffer->vec-of-nodes! (mpar/buffers @thread-num))]
    ["thread: " @thread-num nodes]))

(deftest test-threads
  (testing "thread creation and buffer interaction"
    (let [bodies (mpar/create-thread-bodies mp/nthreads test-algo)
          vec-of-nodes (make-vec-of-Nodes 4)
          _ (mpar/put-buffer! (mpar/buffers 0) vec-of-nodes)
          results (into [] (map #(future %) bodies))
          result0 (results 0)]
      (is (= 0 (@result0 1)))
      (is (= mp/nthreads (count (@result0 2)))) ) ))

(deftest test-get-open
  (testing "get open queue from thread mp")
  (let [tp (mpar/create-thread-params)
        pq (mpar/get-open tp)]
    (is (not (nil? pq)))))

(deftest test-create-expanders
  (testing 
    "creates nthread expanders with start-node in proper receptacle"
    (let [start-node (mb/start-node)
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
    (let [start (mb/start-node)
          succs (mpar/make-successor-nodes start)]
      (is (= (count succs) 3)))))

(deftest test-buffer-load
  (testing "loading of buffers: count out should = count in")
  (let [buffers (mpar/create-buffers mp/nthreads)
        vec-of-nodes (make-vec-of-Nodes 10)]
    (mpar/add-to-buffers! buffers vec-of-nodes)
    (is (= (count vec-of-nodes) (reduce + (map #(count (deref %)) buffers))))))

(deftest test-put-closed
  (testing "add node to closed map in thread-local var closed"
    (let [node (make-dummy-Node)
          f (with-local-vars [closed (hash-map)]
              (mpar/put-closed closed node)
              @closed)
          fut (future f)
          loc (:loc node)]
      (is (= node (get @fut loc) )))))
