(ns maze.paral
  (:require [maze.params :as mp]
            [maze.utils :as mu]
            [maze.base :as mb]
            [maze.overlay :as mo]))

;; implementing algos from Fukunga, Botea, et al.

(def incumbent (atom {:node nil :cost Integer/MAX_VALUE}))

(defn compute-recipient
  "compute recipient of Node based on hash of its .loc
    Want all nodes with same loc to be processed by same thread"
  [node]
  (mod (hash (:loc node)) mp/nthreads))

(def buffers [])
(def clocks [])
(def counters [])
(def tmaxes [])
(def ctrl-msgs [])
(def ctrl-wave-in-progress? (atom false))
(def should-terminate? (atom false))

(defmacro create-thing
  [n create-fn init-val]
  `(mapv ~create-fn (repeat ~n ~init-val)))

(defn reset-all
  "reset buffers and counters"
  []
  (swap! incumbent merge {:cost Integer/MAX_VALUE :node nil})
  (swap! ctrl-wave-in-progress? (constantly false))
  (swap! should-terminate? (constantly false))
  (doseq [[sym create-fn init] [[#'buffers atom #{}] [#'counters atom 0]
                                [#'clocks atom 0] [#'tmaxes atom 0]
                                [#'ctrl-msgs atom []]]]
    (alter-var-root sym (constantly (create-thing mp/nthreads create-fn init)))))

;; puts and takes to thread buffer are 'timestamped messages' in the sense of Mattern paper
;; A mesg is a vector [CLOCK, node]
;; this is SEND in Mattern's terminology
(defn put-buffer
  "put a node from thread 'sender' (thread-num) in buffer[compute-recipient(node)]
    update counter and clock for the sender"
  [node sender]
  (let [receiver (compute-recipient node)
        buffer (buffers receiver)
        counter (counters sender)
        clock (clocks sender)]
    ;; implements send portion of Mattern protocol, lines 1-2, p. 166
    #_(swap! clock inc)
    (swap! counter inc)
    (swap! buffer conj [@clock node])))

(defn put-vec-to-buffer
  "put a vector of nodes in buffers[compute-recipient(node)], update send-counters[i]"
  [nodes sender]
  {:pre [(vector? nodes)]}
  (doseq [node nodes]
    (put-buffer node sender)))

(defn take-buffer
  "get and remove first node from numbered buffer; return nil if buffer empty;
    update counter and tmax for the receiver"
  [receiver]
  ;; a msg is a vector [tstamp node], per Mattern paper
  (let [buffer (buffers receiver)
        counter (counters receiver)
        tmax (tmaxes receiver)
        msg (first @buffer)
        [tstamp node] msg ]
    ;; tmax for the receiver is set to max of tmax and time stamp
    ;; counter is decremented to register receipt
    (when node
      (swap! buffer disj msg)
      (swap! counter dec)
      (swap! tmax max tstamp))
    node))

(defn put-open
  "add [clock+1 node] msg to open (msg priority) queue; update counter"
  [open node thread-num]
  (let [counter (counters thread-num)
        clock (clocks thread-num)]
    ;; implements send portion of Mattern protocol, lines 1-2, p. 166
    #_(swap! clock inc)
    (swap! counter inc)
    (mb/add-nodes! open [[@clock node]]))
  
  (defn take-open
    "take lowest-cost msg [clock node] from open queue"
    [open thread-num]
    (let [counter (counters thread-num)
          tmax (tmaxes thread-num)
          [tstamp node] (mb/get-next! open)]
      (when node
        (swap! counter dec)
        (swap! tmax max tstamp))
      node)))

(defmacro setup-future
  "set up ith future"
  [i func]
  `(future (let [closed# (atom (hash-map))
                 ;; don't need atom here because priq is mutable
                 open# (mb/new-msg-priq [] 100)
                 thread-num# ~i]
             (~func closed# open# thread-num#))))

(defn create-futures
  "create n futures from setup-thread; func(closed, open, thread-num)"
  [n func]
  (into [] (for [i (range n)] (setup-future i func))))

(defn make-successor-nodes
  "make successors to the current node"
  [node]
  (let [loc (.loc node)
        newg (inc (.g node))
        succs (mu/successors loc)
        goal @mp/goal*]
    ;; parent :loc becomes parent-loc of successor node
    (into [] (for [s succs]
               (mb/->Node s (:loc node) newg (mb/calc-heuristic s goal))))))

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
  [closed node]
  (let [loc (:loc node)]
    (get @closed loc)))

;;--------------termination detection--------------
;; for details of termination algorithm, see Mattern 1987 paper, section 6, p. 166
(defn next-recip
  "return the next agent to receive a control message"
  [j]
  (let [next-thread (mod (inc j) mp/nthreads)]
    (ctrl-msgs next-thread)))

;; Mattern p 167 steps 13-14
(defn initiate-ctrl-wave
  "initiate control wave from thread 0 if goal has been reached
    and no control wave already in progress"
  []
  (when (and (not @ctrl-wave-in-progress?)
             (:node @incumbent))
    (do 
      (swap! ctrl-wave-in-progress? (constantly true))
      (let [clock (swap! (clocks 0) inc)
            count @(counters 0)
            msg [clock count false 0]]
        (swap! (next-recip 0) conj msg)))))

(defn process-ctrl-msg
  "process control msg from thread j"
  [j]
  ;; msg format is [time accu invalid init]
  (if @ctrl-wave-in-progress?
    (let [rcvr (ctrl-msgs j)]
      (if-let [msg (peek @rcvr)]
        (do
          (swap! rcvr pop)
          (let [clock (clocks j)
                [time accu invalid init] msg]
            #_(println "rcvd msg" msg "init" init)
            (swap! clock max time)
            ;; check for complete round; initiating thread is always 0
            (if (= init j)
              (do
                (swap! ctrl-wave-in-progress? (constantly false))
                (if (and (zero? accu)
                         (not invalid))
                  :terminated
                  :continue))
              (let [count @(counters j)
                    tmax @(tmaxes j)
                    new-invalid (or invalid (>= tmax time))]
                #_(println "sending to" (next-recip j))
                ;; pass message on to next recipient
                (swap! (next-recip j) conj [time (+ accu count) new-invalid init])
                :continue))))
        :continue))
    :continue))

(defn termination-detector
  "term detection functions to run in thread"
  []
  (loop [stop? false]
    (if stop?
      (swap! should-terminate? (constantly true))
      (do 
        (when (not @ctrl-wave-in-progress?)
          (initiate-ctrl-wave))
        ;; read ctrl msgs for each thread
        (let [ctrls (mapv process-ctrl-msg (range mp/nthreads))
              stop? (= :terminated (ctrls 0))]
          (recur stop?))))))

(defn create-termination-detector
  "run the termination detection algo in its own thread"
  []
  (future-call termination-detector))

;; end of termination detection ----------------------------------

(defn process-successors
  "process the successor nodes to node"
  [node thread-num]
  (let [succs (make-successor-nodes node)]
    (doseq [succ succs]
      (put-buffer succ thread-num))))

(defn expand-open
  "take next node from open Frontier on thread and expand it"
  [closed open thread-num]
  (when (not (mb/deserted? open))
    (let [node (take-open open thread-num)
          current-cost (mb/node-total-cost node)
          incumbent-cost (:cost @incumbent)]
      (put-closed closed node)
      (if (mu/at-goal? (:loc node))
        (when (< current-cost incumbent-cost)
          (swap! incumbent merge {:cost current-cost :node node}))
        ;; no point expanding nodes that are already over cost-limit
        (when (< (inc current-cost) incumbent-cost)
          (process-successors node thread-num))))))

(defn intake-from-buff
  [closed open thread-num]
  #_(log thread-num "intake")
  (when-let [n' (take-buffer thread-num)]
    #_(log thread-num "non-nil take" n')
    (if-let [oldn (find-in-closed closed n')]
      (let [g1 (:g n')
            oldg (:g oldn)]
        (when (< g1 oldg)
          (remove-from-closed closed oldn)
          (put-open open n' thread-num)))
      (do 
        #_(log thread-num "Putting" n')
        (put-open open n' thread-num))))
  [closed open thread-num])

(defn dpa
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [closed open thread-num]
  (while (not @should-terminate?)
    (intake-from-buff closed open thread-num)
    (expand-open closed open thread-num))
  :terminated
  )

;; dpa development version, using dotimes instead of while
(defn xdpa
  [closed open thread-num]
  (dotimes [t 1]
    (mu/log thread-num "starting")
    (do
      #_(log thread-num "calling intake")
      (intake-from-buff closed open thread-num)
      (expand-open closed open thread-num)))
  (mb/deserted? open)
  #_[closed open thread-num])

(defn init-run
  "start the run by placing start node in buffers[0]"
  []
  (reset-all)
  (let [snode (mb/start-node)]
    (put-buffer snode 0)))

(defmacro readout
  "stringify readout of var"
  [v]
  `(if (vector? ~v)
     (str (name (quote ~v)) ": " (mapv deref ~v))
     (str (name (quote ~v)) ": " @~v)))

(defn xstatus
  "returns vector of strings giving readout of quoted vars specified in xs;
    the xs must be derefable"
  [xs]
  (mapv eval 
        (into []
              (partition 2 (interleave (repeatedly (constantly  `readout)) xs)))))

(defn pstatus
  "prints out the results of xstatus in human-readable form for key variables of the algo"
  []
  (let [xs [`buffers `counters `clocks `tmaxes `ctrl-msgs `ctrl-wave-in-progress?
            `should-terminate? `incumbent]]
    (doseq [line (xstatus xs)]
      (println line))
    (println "---------")))

(defn pextract-path
  [goal-node]
  (loop [path []
         node goal-node]
    (if (nil? node)
      path
      (recur (conj path (:loc node)) (:parent node)))))

(defn finish-up
  [doprint]
  (let [path (pextract-path (:node @incumbent))]
    (if (= {:cost @incumbent} Integer/MAX_VALUE)
      (println "No path found")
      (if doprint
        (do
          (doseq [ln (mo/a-overlay-path path)]
            (println ln))
          (println "Found: Path length: " (count path))
          (mu/print-maze-params))
        (println "Path length:" (count path))))))

;; https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
(defn psearch-start
  "start a new || astar search; print path overlaid result if doprint is true"
  ([]
   (psearch-start true))
  ([doprint]
   (reset-all)
   (init-run)
   (println "Searching maze")
   (let [rets (create-futures mp/nthreads dpa)
         res (mapv #(deref % 5000 :timedout) rets)
         term-detector (create-termination-detector)]
     (if @should-terminate?
       (do
         (println "All terminated")
         (finish-up doprint))
       (do
         (println "Error:" res)
         (map future-cancel rets)
         (future-cancel term-detector)
         (pstatus))))))


