(ns maze.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [maze.base :as mb]
            [maze.core :as mc]
            [maze.paral :as mpar]
            [maze.params :as mp]
            [test-utils :as tu]))

(defn setup-trivial-maze
  []
  (mc/read-maze "trivial4"))


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
    (let [vec-of-nodes (tu/make-vec-of-nodes 100)
          _ (mpar/put-vec-to-buffer vec-of-nodes 0)
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



(deftest test-get-buffer
  (testing "testing get from numbered buffer"
    (setup-trivial-test)
    (let [nodes (into [] (map mpar/take-buffer (range mp/nthreads)))
          start-in-buff? (some #(= (mb/start-node) %) nodes)]
      (is (true? start-in-buff?))
      (is (= 0 (reduce + (mapv deref mpar/counters)))))))



;; this test will fail if mp/nthreads != 4
(deftest test-initial-load
  (testing "loading of start node into open queue"
    (setup-trivial-test)
    (mpar/init-run)
    (is (= [0 (mb/start-node)] (first @(mpar/buffers 3))))
    (let [futs (mpar/create-futures mp/nthreads mpar/xdpa)]
      ;; start node of trivial maze lands in buffer[3] when mp/nthreads=4
      (is (= [true true true true] (mapv deref futs)))
      (is (= [1 1 1 0] (mapv #(count (deref %)) mpar/buffers)))
      (is (= [1 0 0 2] (mapv deref mpar/counters))))))

;; testing termination detection functions

(deftest test-initiate-control-wave
  (testing "initiation of control wave"
    (mpar/reset-all)
    (with-redefs [mpar/incumbent (atom {:node true})]
      (mpar/initiate-ctrl-wave)
      (is (= 1 @(mpar/clocks 0)) "initiator clock increment")
      (is (= [1 0 false 0] (peek @(mpar/ctrl-msgs 1))) "message rcvd in next message buffer"))))

(deftest test-process-ctrl-msg
  (testing "processing of control messages"
    (mpar/reset-all)
    (with-redefs [mpar/incumbent (atom {:node true})]
      (mpar/initiate-ctrl-wave)
      (doseq [j (range 1 mp/nthreads)]
        (is (= :continue (mpar/process-ctrl-msg j))))
      (is (= [1 0 false 0] (peek @(mpar/ctrl-msgs 0))))
      (is (= :terminated (mpar/process-ctrl-msg 0))))))

(deftest test-msg-queue
  (testing "priority queue of time-stamped messages"
    (setup-trivial-maze)
    (let [mpq (mb/new-msg-priq [] 100)
          test-node (mb/start-node)
          bigger-node (mb/->Node [10 20] test-node 100 200)]
      (mb/add-nodes! mpq [[0 test-node] [0 bigger-node]])
      (is (= [0 test-node] (mb/get-next! mpq)))
      (is (= [0 bigger-node] (mb/get-next! mpq))))))

(deftest test-put-take-open
  (testing "frontier management functions in parallel case"
    (mpar/reset-all)
    (setup-trivial-maze)
    (let [open (mb/new-msg-priq [] 100)
          node (mb/start-node)]
      (mpar/put-open open node 0)
      (is (= 1 @(mpar/counters 0)))
      (is (= node (mpar/take-open open 0)))
      (is (= 0 @(mpar/counters 0)))
      (is (= 0 @(mpar/tmaxes 0)))
      (is (nil? (mpar/take-open open 0))))))


