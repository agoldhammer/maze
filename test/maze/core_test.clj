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
  (let [nodes (mpar/buffer->vec-of-nodes! thread-num)]
    (count nodes)))

(defn test-algo2
  [closed open thread-num]
  (let [nodes (mpar/buffer->vec-of-nodes! thread-num)]
    (when (seq nodes)
      (mpar/put-closed closed (first nodes))))
  @closed)

(deftest test-threads
  (testing "thread creation and buffer interaction"
    (mpar/reset-all)
    (let [vec-of-nodes (make-vec-of-Nodes 4)
          _ (mpar/put-vec-to-buffer vec-of-nodes)
          futs (mpar/create-futures mp/nthreads test-algo)
          num-nodes-read (reduce + (map deref futs))]
      (is (= 4 num-nodes-read)))))

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
    (is (= (count vec-of-nodes) (mpar/sum-counters mpar/send-counters)))))

(deftest test-get-buffer
  (testing "testing get from numbered buffer"
    (mpar/reset-all)
    (setup-trivial-maze)
    (mpar/init-run)
    (let [nodes (into [] (map mpar/take-buffer (range mp/nthreads)))
          start-in-buff? (some #(= (mb/start-node) %) nodes)]
      (is (true? start-in-buff?)))))

(deftest test-counters
  (testing "send-receive counters"
    (mpar/reset-counters)
    (mpar/inc-counter mpar/send-counters 0 5)
    (mpar/inc-counter mpar/send-counters 1 3)
    (mpar/inc-counter mpar/recv-counters 1 2)
    (mpar/inc-counter mpar/recv-counters 2 1)
    (is (= 8 (mpar/sum-counters mpar/send-counters)))
    (is (= 3 (mpar/sum-counters mpar/recv-counters)))))

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
