(ns maze.buffers
  #_(:require [maze.params :as mp])
  (:require [maze.base :as mb])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defprotocol Buffer
  "protocol for counting buffer"
  (get-count [this] "return count of msgs in buffer")
  (get-tmax [this] "return tmax for this buffer")
  (get-clock [this] "return the clock")
  (set-clock [this time] "set the clock to `time`")
  (take-buff [this] "take next basic msg")
  (put-buff [this payload] "add basic msg")
  (put-coll-buff [this coll] "put-buff on each member of coll")
  (vide? [this] "buffer empty?")
  (quickpeek [this] "Peek at next"))

(deftype CountedBuffer [buff clock tmax])

(defn new-counted-buffer
  "returns new Counted-Buffer"
  []
  (->CountedBuffer (LinkedBlockingQueue.) (atom 0) (atom 0)))

(extend-protocol Buffer
  CountedBuffer
  (get-count [this]
    (.size (.buff this)))
  (get-tmax [this]
    @(.tmax this))
  (get-clock [this]
    @(.clock this))
  (set-clock [this time]
    (reset! (.clock this) time))
  (take-buff [this]
    (let [[clock payload] (.take (.buff this))]
      (swap! (.tmax this) max clock)
      [clock payload]))
  (put-buff [this payload]
    (.put (.buff this) [@(.clock this) payload]))
  (put-coll-buff [this coll]
    {:pre [(seqable? coll)]}
    (doseq [x coll]
      (put-buff this x)))
  (vide? [this]
    (.isEmpty (.buff this)))
  (quickpeek [this]
    (.peek (.buff this))))

(defn compute-recipient
  "compute recipient of Node based on hash of its .loc
    Want all nodes with same loc to be processed by same thread"
  [node nthreads]
  (mod (hash (:loc node)) nthreads))


;;;;;;;;;;for testing

(defn make-dummy-Node
  "make a dummy node"
  []
  (let [x (rand-int 100)
        y (rand-int 100)
        g (rand-int 50)
        h (rand-int 1000)]
    ;; loc parent g h
    (apply mb/->Node [[x y] [(inc x) y] g h])))

(def cbuffs (into [] (repeatedly 4 new-counted-buffer)))

(def flag (promise))

(defn make-vec-of-nodes
  [n]
  (into [] (repeatedly n make-dummy-Node)))

(defn feed-buffs
  [n]
  (when (= @flag 42)
    (let [v (make-vec-of-nodes n)]
      (doseq [node v]
        (put-buff (cbuffs (compute-recipient node 4)) node))
      (mapv get-count cbuffs))))

(defn pump-buff
  [i]
  (future 
   (let [buff (cbuffs i)]
     (loop [accum []]
       (if (vide? buff)
         accum
         (recur (conj accum (take-buff buff))))))))

(defn pump-buffs
  []
  (when (= @flag 42)
    (let [contents (mapv pump-buff (range 4))]
      (while (not (every? zero? (mapv get-count cbuffs)))
        (Thread/sleep 1))
      (mapv deref contents))))

(comment
  (def cbuffs (into [] (repeatedly 4 new-counted-buffer)))
  (def flag (promise))
  (def feed (future (feed-buffs 20)))
  (deliver flag 42)
  (mapv get-count cbuffs)
  (def pumps (future-call pump-buffs)))





