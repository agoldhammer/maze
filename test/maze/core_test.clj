(ns maze.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [maze.base :as mb]
            [maze.core :as mc]
            [maze.paral :as mpar]
            [maze.params :as mp]))

(defn setup-trivial-maze
  []
  (mc/read-maze "trivial4"))

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

(defn setup-trivial-test
  []
  (mpar/reset-all)
  (setup-trivial-maze)
  (mpar/init-run))

(defn test-algo
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [closed open thread-num]
  (let [i (atom 0)]
    (while (mpar/take-buffer thread-num)
      (swap! i inc))
    @i))

(defn test-algo2
  [closed open thread-num]
  (let [node (mpar/take-buffer thread-num)]
    (when node
      (mpar/put-closed closed node)))
  @closed)

(deftest test-threads
  (testing "thread creation and buffer interaction"
    (mpar/reset-all)
    (let [vec-of-nodes (make-vec-of-Nodes 100)
          _ (mpar/put-vec-to-buffer vec-of-nodes)
          futs (mpar/create-futures mp/nthreads test-algo)
          num-nodes-read (reduce + (map deref futs))]
      (is (= 100 num-nodes-read)))))

(deftest test-threads2
  (testing "get from buff and put in closed"
    (mpar/reset-all)
    ; start node needs to be set
    (setup-trivial-maze)
    (mpar/init-run)
    (let [futs (mpar/create-futures mp/nthreads test-algo2)
          vals (map deref futs)
          snode (mb/start-node)]
      ;; this works only because start of trivial maze lands in buffers[3]
      ;; when nthreads = 4; FIX to work in general case
      (is (= {(:loc snode) snode} (last vals))))))

(deftest test-trivial
  (testing "trivial maze"
    (setup-trivial-maze)
    (is (=  @mp/start* [1 0]))
    (let [start (mb/start-node)
          succs (mpar/make-successor-nodes start)]
      (is (= (count succs) 3)))))

(deftest test-buffer-load
  (testing "loading of buffers: count out should = count in")
  (let [vec-of-nodes (make-vec-of-Nodes 100)]
    (mpar/reset-all)
    (mpar/put-vec-to-buffer vec-of-nodes)
    (is (= (count vec-of-nodes) (mpar/balance-counters)))))

(deftest test-get-buffer
  (testing "testing get from numbered buffer"
    (setup-trivial-test)
    (let [nodes (into [] (map mpar/take-buffer (range mp/nthreads)))
          start-in-buff? (some #(= (mb/start-node) %) nodes)]
      (is (true? start-in-buff?)))))

(deftest test-closed-functions
  (testing "functions dealing with closed map"
    (let [closed (atom {})
          node (make-dummy-Node)]
      (mpar/put-closed closed node)
      (is (= node (mpar/find-in-closed closed node)))
      (mpar/remove-from-closed closed node)
      (is (nil? (mpar/find-in-closed closed node))))))

(deftest test-open-functions
  (testing "open functions"
    (let [open (mb/new-priq [] 100)
          node (make-dummy-Node)]
      (mpar/put-open open node)
      (is (= node (mb/quickpeek open))))))

;; this test will fail if mp/nthreads != 4
(deftest test-initial-load
  (testing "loading of start node into open queue"
    (setup-trivial-test)
    (mpar/init-run)
    (is (= (mb/start-node) (first @(mpar/buffers 3))))
    (let [futs (mpar/create-futures mp/nthreads mpar/xdpa)]
      ;; start node of trivial maze lands in buffer[3] when mp/nthreads=4
      (is (= [true true true true] (mapv deref futs)))
      (is (= [1 1 1 0] (mapv #(count (deref %)) mpar/buffers)))
      (is (= [1 1 1 0] (mapv deref mpar/counters))))))


