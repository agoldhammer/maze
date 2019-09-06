(ns maze.buffers-test
  (:require [clojure.test :refer [deftest is are testing use-fixtures]]
            [maze.buffers :as mbuff]
            [maze.base :as mb]
            [taoensso.timbre :as log]
            #_[maze.core :as mc]
            #_[maze.paral :as mpar]
            [maze.params :as mp]))

(def flag (promise))
(def can-finish? (atom false))
(def cbuffs (into [] (repeatedly 4 mbuff/new-counted-buffer)))
(def should-terminate? (atom false))

(defn make-dummy-node
  "make a dummy node"
  []
  (let [x (rand-int 100)
        y (rand-int 100)
        g (rand-int 50)
        h (rand-int 1000)]
    ;; loc parent g h
    (apply mb/->Node [[x y] [(inc x) y] g h])))

(defn make-vec-of-nodes
  [n]
  (into [] (repeatedly n make-dummy-node)))


(defn feed-buffs
  [n]
  (when (= @flag 42)
    (let [v (make-vec-of-nodes n)]
      (doseq [node v]
        (mbuff/put-buff (cbuffs (mbuff/compute-recipient node 4)) [0 node]))
      (swap! can-finish? not)
      (mapv mbuff/get-count cbuffs))))

(defn pump-buff
  [i]
  (future
    (let [buff (cbuffs i)]
      (loop [accum []]
        (if-let [msg (mbuff/take-buff buff)]
          (do
            (log/infof "buff num: %s msg: %s" i msg)
            (if (= (msg 1) :finish)
              accum
              (recur (conj accum msg))))
          accum)))))

(defn pump-buffs
  []
  (when (= @flag 42)
    (let [future-contents
          (mapv pump-buff (range 4))]
      (loop [counts (mapv mbuff/get-count cbuffs)]
        (if (and @can-finish?
                 (every? zero? counts))
          (mapv #(mbuff/put-buff (cbuffs %) [0 :finish]) (range 4))
          (do
            (Thread/sleep 1)
            (recur (mapv mbuff/get-count cbuffs)))))
      future-contents)))

(defn reset-test-env
  []
  (swap! should-terminate? (constantly false))
  (swap! can-finish? (constantly false))

  (alter-var-root #'cbuffs (constantly (into [] (repeatedly 4 mbuff/new-counted-buffer))))
  (alter-var-root #'flag (constantly (promise))))

(defn tst
  []
  (reset-test-env)
  #_(alter-var-root #'feeds (constantly (future (feed-buffs 1200))))
  #_(alter-var-root #'pumps (constantly (future-call pump-buffs)))
  (let [feed (future (feed-buffs 20))]
    (deliver flag 42)
    (Thread/sleep 1)
    (println "cbuff counts" (mapv mbuff/get-count cbuffs))
    feed)
  #_(println "pumps" pumps))


(deftest test-buff-put-take
  (testing "puts and takes"
    (reset-test-env)
    (future (feed-buffs 20))
    (deliver flag 42)
    (Thread/sleep 5)
    (let [load-counts (mapv mbuff/get-count cbuffs)
          res (pump-buffs)]
      (is (= load-counts (mapv count (mapv deref res)))))))

(defn feed-input-buffs
  [n]
  (mbuff/reset-all)
  (log/info "starting")
  (let [v (make-vec-of-nodes n)]
    (doseq [node v]
      (let [msg [0 node]
            recip (mbuff/compute-recipient msg mp/nthreads)
            buff (mbuff/input-buffs recip)]
        (log/infof "Sending to inbuff %s: %s" recip msg)
        (mbuff/put-buff buff msg)))
    (dotimes [i mp/nthreads]
      (loop [msg (mbuff/poll-buff (mbuff/input-buffs i))]
        (when msg
          (log/infof "sending to open-q %s: %s" i msg)
          (mbuff/put-buff (mbuff/open-qs i) msg)
          (recur (mbuff/poll-buff (mbuff/input-buffs
                                   (mbuff/compute-recipient msg mp/nthreads)))))))
    (dotimes [i mp/nthreads]
      (mbuff/put-buff (mbuff/open-qs i) [5 (mb/->Node [0 1] [0 1] Integer/MAX_VALUE 0)]))
    
    (mapv mbuff/get-count mbuff/open-qs)))


(defmacro while-let
  "Repeatedly executes body while test expression is true, evaluating the body with binding-form bound to the value of test."
  [[form test] & body]
  `(loop [~form ~test]
     (when ~form
       ~@body
       (recur ~test))))

(defn safe-println [& more]
  (.write *out* (str (clojure.string/join " " more) "\n")))

(defn readout-buff
  [i]
  (let [buff (mbuff/open-qs i)]
    (log/infof "buff %s" buff)
    (log/info "before")
    (log/spy (mbuff/quickpeek buff))
    (future
      (log/info "starting readout")
      (loop [msg (mbuff/take-buff buff)
             accum []]
        (log/infof "msg: %s" msg)
        (if (= (msg 0) 5)
          accum
          (do
            (log/infof "readout %s" msg)
            (recur (mbuff/take-buff buff) (conj accum msg))))))))

(defn wtf
  []
  (log/info "starting")
  (future 
   (log/info "future")
   (dotimes [i 10]
     (log/infof "ith round %s" i)
     (Thread/sleep (rand-int 500)))
   :done))