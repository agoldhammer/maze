(ns maze.paral-test
  (:require [clojure.test :refer [deftest is are testing use-fixtures]]
            [maze.test-utils :as tu]
            [maze.buffers :as mbuff]
            #_[maze.base :as mb]
            #_[taoensso.timbre :as log]
            #_[maze.core :as mc]
            [maze.paral :as mpar]
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
      (mpar/put-closed closed node)
      (is (= node (mpar/find-in-closed closed node)))
      (mpar/remove-from-closed closed node)
      (is (nil? (mpar/find-in-closed closed node))))))
