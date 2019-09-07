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




