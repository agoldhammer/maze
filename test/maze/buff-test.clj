(ns maze.buffers-test
  (:require [clojure.test :refer [deftest is are testing use-fixtures]]
            [maze.buffers :as mbuff]
            #_[maze.base :as mb]
            #_[maze.core :as mc]
            #_[maze.paral :as mpar]
            #_[maze.params :as mp]))

(deftest test-buff-put-take
  (testing "puts and takes"
    (mbuff/reset-all)
    (future (mbuff/feed-buffs 20))
    (deliver mbuff/flag 42)
    (Thread/sleep 5)
    (let [load-counts (mapv mbuff/get-count mbuff/cbuffs)
          res (mbuff/pump-buffs)]
      (is (= load-counts (mapv count (mapv deref res)))))))


