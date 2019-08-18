(ns maze.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [maze.core :as mc]
            [maze.paral :as mp]
            [maze.params :as mparms]))

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
    (apply mc/->Node [[x y] [(inc x) y] g h])))

(defn make-sequence-of-Nodes
  [n]
  (repeatedly n make-dummy-Node))

(defn make-test-pq
  "make a PriQ for testing"
  [n]
  (let [queue (mc/priority-queue 1000 mc/node-comp)
        pq (mc/->PriQ queue)]
    (mc/add-nodes! pq (make-sequence-of-Nodes n))
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
    (let [start mc/astar-start
          thr (mp/create-thread-params start)
          pq (:open thr)
          node (mc/get-next! pq)]
      (is (= node start))))
  (testing "thread creation empty"
    (let [thr (mp/create-thread-params)]
      (is (mc/deserted? (:open thr))))))


(deftest test-buffer-next-and-put-buffer
  (testing "retrieve from buffer with something in it"
    (let [buffers (mp/create-buffers mparms/nthreads)
          buffer (nth buffers 0)
          in-tuple [[0 1] [0 0] 1 1]
          node (apply mc/->Node in-tuple)]
      (mp/put-buffer buffer node)
      (let [out-node (mp/buffer-next buffer)]
        (is (= out-node node))
        (is (nil? (mp/buffer-next buffer)))))))

(deftest test-get-open
  (testing "get open queue from thread parms")
  (let [tp (mp/create-thread-params)
        pq (mp/get-open tp)]
    (is (not (nil? pq)))))

(deftest test-create-expanders
  (testing 
   "creates nthread expanders with start-node in proper receptacle"
    (let [start-node (mc/astar-start)
          expanders (mp/create-expanders mparms/nthreads start-node)
          start-recipient (mp/compute-recipient start-node)
          nth-tp (nth expanders start-recipient)
          pq (mp/get-open nth-tp)]
      (is (not (nil? pq)))
      (is (< start-recipient (count expanders)) )
      (is (not (nil? nth-tp)))
      (is (= (mc/countf (mp/get-open (nth expanders start-recipient))) 1) ))))

(deftest test-trivial
  (testing "trivial maze"
    (setup-trivial-maze)
    (is (= @mparms/start* [1 0]))
    (let [start (mc/start-Node)
          succs (mp/make-successor-nodes start)]
      (is (= (count succs) 3)))))



