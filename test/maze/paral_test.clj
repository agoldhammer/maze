(ns maze.paral-test
  (:require [clojure.test :refer [deftest is are testing use-fixtures]]
            [maze.test-utils :as tu]
            [maze.buffers :as mbuff]
            [maze.base :as mb]
            [maze.paral :as mpar]
            [maze.params :as mp]))

(defn setup-trivial-test
  []
  (mbuff/reset-all)
  (tu/setup-trivial-maze)
  #_(mpar/init-run))

(deftest test-closed-functions
  (testing "functions dealing with closed map"
    (mpar/reset-all)
    (let [closed (atom {})
          node (tu/make-dummy-node)
          ibuff (mbuff/hash-of-loc (:loc node) mp/nthreads)]
      (mbuff/put-closed closed node)
      (is (= node (mbuff/find-in-closed closed (:loc node))))
      (mbuff/remove-from-closed closed node)
      (is (nil? (mbuff/find-in-closed closed (:loc node))))
      (mbuff/put-closed (mbuff/closed-locs ibuff) node)
      (is (= node (mbuff/find-in-closed (:loc node)))))))

(deftest test-process-successors
  (testing "routing of successors for trivial maze"
    (setup-trivial-test)
    (mpar/process-successors (mb/start-node) (mbuff/open-qs 0))
    (let [v (mapv mbuff/poll-buff mbuff/input-buffs)]
      (is (= [2 0] (:loc (get-in v [0 1]))))
      (is (= [1 1] (:loc (get-in v [1 1]))))
      (is (= [0 0] (:loc (get-in v [2 1]))))
      (is (nil? (v 3))))))

(deftest test-intake
  (testing "intake from input buffers"
    (setup-trivial-test)
    (let [ibuff (mbuff/hash-of-loc (:loc (mb/start-node)) mp/nthreads)
          input (mbuff/input-buffs ibuff)
          closed (mbuff/closed-locs ibuff)
          open (mbuff/open-qs ibuff)] ; ibuff will equal 3
      (mbuff/put-buff input [0 (mb/start-node)])
      (mpar/intake-from-buff input closed open)
      (is (= [0 (mb/start-node)] (mbuff/poll-buff open))))))

(deftest test-expand-open
  (testing "expand-open"
    (setup-trivial-test)
    (let [open (mbuff/open-qs 0)
          closed (mbuff/closed-locs 0)]
      (mbuff/put-buff open [0 (mb/start-node)])
      (mpar/expand-open closed open)
      (let [v (mapv mbuff/poll-buff mbuff/input-buffs)]
        (is (= [2 0] (:loc (get-in v [0 1]))))
        (is (= [1 1] (:loc (get-in v [1 1]))))
        (is (= [0 0] (:loc (get-in v [2 1]))))
        (is (nil? (v 3))))
      (let [closed-node (mbuff/find-in-closed closed (:loc (mb/start-node)))]
        (is (= (mb/start-node) closed-node))))))
