(ns maze.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [maze.base :as mb]
            #_[maze.core :as mc]
            [maze.paral :as mpar]
            [maze.params :as mp]
            [maze.test-utils :as tu]))

(deftest test-trivial
  (testing "trivial maze"
    (tu/setup-trivial-maze)
    (is (=  mp/start* [1 0]))
    (let [start (mb/start-node)
          succs (mpar/make-successor-nodes start)]
      (is (= (count succs) 3)))))

;; testing termination detection functions

#_(deftest test-initiate-control-wave
  (testing "initiation of control wave"
    (mpar/reset-all)
    (with-redefs [mpar/incumbent (atom {:node true})]
      (mpar/initiate-ctrl-wave)
      (is (= 1 @(mpar/clocks 0)) "initiator clock increment")
      (is (= [1 0 false 0] (peek @(mpar/ctrl-msgs 1))) "message rcvd in next message buffer"))))

#_(deftest test-process-ctrl-msg
  (testing "processing of control messages"
    (mpar/reset-all)
    (with-redefs [mpar/incumbent (atom {:node true})]
      (mpar/initiate-ctrl-wave)
      (doseq [j (range 1 mp/nthreads)]
        (is (= :continue (mpar/process-ctrl-msg j))))
      (is (= [1 0 false 0] (peek @(mpar/ctrl-msgs 0))))
      (is (= :terminated (mpar/process-ctrl-msg 0))))))




