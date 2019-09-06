(ns maze.paral-test
  (:require [clojure.test :refer [deftest is are testing use-fixtures]]
            [maze.test-utils :as tu]
            [maze.buffers :as mbuff]
            [maze.base :as mb]
            [maze.paral :as mpar]
            #_[taoensso.timbre :as log]
            #_[maze.core :as mc]
            #_[maze.params :as mp]))

(defn setup-trivial-test
  []
  (mbuff/reset-all)
  (tu/setup-trivial-maze)
  #_(mpar/init-run))

(deftest test-closed-functions
  (testing "functions dealing with closed map"
    (let [closed (atom {})
          node (tu/make-dummy-node)]
      (mbuff/put-closed closed node)
      (is (= node (mbuff/find-in-closed closed (:loc node))))
      (mbuff/remove-from-closed closed node)
      (is (nil? (mbuff/find-in-closed closed (:loc node)))))))

(deftest test-process-successors
  (testing "routing of successors for trivial maze"
    (setup-trivial-test)
    (mpar/process-successors (mb/start-node) 0)
    (let [v (mapv mbuff/poll-buff mbuff/input-buffs)]
      (is (= [2 0] (:loc (get-in v [0 1]))))
      (is (= [1 1] (:loc (get-in v [1 1]))))
      (is (= [0 0] (:loc (get-in v [2 1]))))
      (is (nil? (v 3))))))
