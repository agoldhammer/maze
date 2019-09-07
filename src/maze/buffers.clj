(ns maze.buffers
  #_(:require [maze.params :as mp])
  (:require [maze.base :as mb]
            [maze.params :as mp])
  #_(:require [maze.utils :as mu])
  #_(:require [taoensso.timbre :as log])
  #_(:require [clojure.tools.logging :as log])
  (:import [java.util.concurrent PriorityBlockingQueue]
           [java.util.concurrent LinkedBlockingQueue]))

(defprotocol Buffer
  "protocol for counting buffer"
  (get-count [this] "return count of msgs in buffer")
  (get-tmax [this] "return tmax for this buffer")
  (get-clock [this] "return the clock")
  (set-clock [this time] "set the clock to `time`")
  (take-buff [this] "take next basic msg, blocking if none")
  (poll-buff [this] "take next basic msg if present")
  (put-buff [this time-stamped-msg] "add basic msg")
  (vide? [this] "buffer empty?")
  (quickpeek [this] "Peek at next"))

(deftype CountedBuffer [buff clock tmax])

(defn new-counted-buffer
  "returns new CountedBuffer with blocking queue"
  []
  (->CountedBuffer (LinkedBlockingQueue.) (atom 0) (atom 0)))

(defn msg-comp
  "compare 2 msgs of form [tstamp nodea] [tstamp nodeb]"
  [[_ nodea] [_ nodeb]]
  (mb/node-comp nodea nodeb))

(defn new-open-queue
  "returns new CountedBuffer with priority queue"
  []
  (->CountedBuffer (PriorityBlockingQueue. 100 (comparator msg-comp)) (atom 0) (atom 0)))

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
  (poll-buff [this]
    (if-let [[clock payload] (.poll (.buff this))]
      (do (swap! (.tmax this) max clock)
          [clock payload])
      nil))
  (put-buff [this time-stamped-msg]
            (.put (.buff this) time-stamped-msg))
  (vide? [this]
         (.isEmpty (.buff this)))
  (quickpeek [this]
             (.peek (.buff this))))

(defn hash-of-loc
  "compute hash of loc of node"
  [loc nthreads]
  (mod (hash loc) nthreads))

(defn compute-recipient
  "compute recipient of tstamped message based on hash of its node's .loc
    Want all nodes with same loc to be processed by same thread"
  [msg nthreads]
  {:pre [(vector? msg)
         (= 2 (count msg))]}
  (let [loc (:loc (msg 1))]
    (hash-of-loc loc nthreads)))

;;; key variables
(def input-buffs [])
(def ctrl-msgs)
(def ctrl-wave-in-progress? (atom false))

(def open-qs [])
(def closed-locs [])
(def incumbent (atom {}))

(defmacro create-thing
  [n create-fn init-val]
  `(mapv ~create-fn (repeat ~n ~init-val)))

(defn reset-all
  "reset buffers and counters"
  []
  (swap! incumbent merge {:cost Integer/MAX_VALUE :node nil})
  (swap! ctrl-wave-in-progress? (constantly false))
  
  (alter-var-root #'input-buffs (constantly (into [] (repeatedly mp/nthreads new-counted-buffer))))
  (alter-var-root #'open-qs (constantly (into [] (repeatedly mp/nthreads new-open-queue))))
  (alter-var-root #'closed-locs (constantly (into [] (repeatedly mp/nthreads #(atom {})))))
  
  #_(doseq [[sym create-fn init] [[#'buffers atom #{}] [#'counters atom 0]
                                  [#'clocks atom 0] [#'tmaxes atom 0]
                                  [#'ctrl-msgs atom []]]]
      (alter-var-root sym (constantly (create-thing mp/nthreads create-fn init)))))

;;;;;;;;; functions for closed-locs
;; `closed` is one of the closed-locs atoms, of which there are mp/threads
;; selection can be made by calling hash-of-loc, as indicated in find-in-closed
;; put and remove interact only from within thread, so know which buffer they are using
;; find-in-closed is used for path extraction: see mpar/pextract

(defn put-closed
  "add node to closed buffer"
  [closed node]
  (swap! closed assoc (:loc node) node))

(defn remove-from-closed
  "remove node from the closed map"
  [closed node]
  (swap! closed dissoc (:loc node)))

(defn find-in-closed
  "return node if in closed, nil otherwise"
  ([loc-or-node]
   (let [loc (if (vector? loc-or-node)
               loc-or-node
               (:loc loc-or-node))
         ibuff (hash-of-loc loc mp/nthreads)
         closed (closed-locs ibuff)]
     (get @closed loc)))
  ([closed loc]
   (get @closed loc)))

(comment
 (def cbuffs (into [] (repeatedly 4 new-counted-buffer)))
  (def flag (promise))
  (def feed (future (feed-buffs 20) ))
  (deliver flag 42)
  (mapv get-count cbuffs)
  (def pumps (future-call pump-buffs))
  
  )