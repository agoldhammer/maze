(ns maze.paral
  (:require [maze.params :as mp]
            [maze.utils :as mu]
            [maze.base :as mb]
            [maze.overlay :as mo]
            [maze.buffers :as mbuff :refer [put-buff take-buff get-clock
                                            remove-from-closed find-in-closed
                                            put-closed vide?]]))

;; implementing algos from Fukunga, Botea, et al.

(def incumbent (atom {:node nil :cost Integer/MAX_VALUE}))

(def ctrl-msgs [])
(def ctrl-wave-in-progress? (atom false))
(def should-terminate? (atom false))

#_(defmacro create-thing
  [n create-fn init-val]
  `(mapv ~create-fn (repeat ~n ~init-val)))

(defn reset-all
  "reset buffers and counters"
  []
  (mbuff/reset-all)
  (swap! incumbent merge {:cost Integer/MAX_VALUE :node nil})
  (swap! ctrl-wave-in-progress? (constantly false))
  (swap! should-terminate? (constantly false))
  #_(doseq [[sym create-fn init] [[#'buffers atom #{}] [#'counters atom 0]
                                  [#'clocks atom 0] [#'tmaxes atom 0]
                                  [#'ctrl-msgs atom []]]]
      (alter-var-root sym (constantly (create-thing mp/nthreads create-fn init)))))

(defmacro setup-future
  "set up ith future"
  [i func]
  `(future-call #(~func (mbuff/input-buffs ~i) (mbuff/closed-locs ~i) (mbuff/open-qs ~i))))

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
        goal mp/goal*]
    ;; parent :loc becomes parent-loc of successor node
    (into [] (for [s succs]
               (mb/->Node s (:loc node) newg (mb/calc-heuristic s goal))))))


;;--------------termination detection--------------
;; for details of termination algorithm, see Mattern 1987 paper, section 6, p. 166
(defn next-recip
  "return the next agent to receive a control message"
  [j]
  (let [next-thread (mod (inc j) mp/nthreads)]
    (ctrl-msgs next-thread)))

;; Mattern p 167 steps 13-14
#_(defn initiate-ctrl-wave
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

#_(defn process-ctrl-msg
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

#_(defn termination-detector
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

#_(defn create-termination-detector
  "run the termination detection algo in its own thread"
  []
  (future-call termination-detector))

;; end of termination detection ----------------------------------

(defn process-successors
  "process the successor nodes to node"
  [node open]
  (let [succs (make-successor-nodes node)
        clock (.clock open)]
    (doseq [succ succs]
      (let [msg [@clock succ]
            ibuf (mbuff/compute-recipient msg mp/nthreads)
            recip (mbuff/input-buffs ibuf)]
        (put-buff recip msg)))))

(defn expand-open
  "take next node from open Frontier on thread and expand it"
  [closed open]
  (when (not (vide? open))
    (let [[_ node] (take-buff open)
          current-cost (mb/node-total-cost node)
          incumbent-cost (:cost @incumbent)]
      (put-closed closed node)
      (if (mu/at-goal? (:loc node))
        (when (< current-cost incumbent-cost)
          (swap! incumbent merge {:cost current-cost :node node}))
        ;; no point expanding nodes that are already over cost-limit
        (when (< (inc current-cost) incumbent-cost)
          (process-successors node open))))))

(defn intake-from-buff
  [input closed open]
  #_(log thread-num "intake")
  (when-let [[_ n'] (take-buff input)]
    #_(log thread-num "non-nil take" n')
    (if-let [oldn (find-in-closed closed (:loc n'))]
      (let [g1 (:g n')
            oldg (:g oldn)]
        (when (< g1 oldg)
          (remove-from-closed closed oldn)
          (put-buff open [(get-clock input) n'])))
      (do
        #_(log thread-num "Putting" n')
        (put-buff open [(get-clock input) n'])))))

(defn dpa
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [input closed open]
  (while (not (:node @incumbent))
    (intake-from-buff input closed open)
    (expand-open closed open))
  :terminated)

;; dpa development version, using dotimes instead of while
(defn xdpa
  [input closed open]
  (dotimes [t 1]
    #_(mu/log thread-num "starting")
    (do
      #_(log thread-num "calling intake")
      (intake-from-buff input closed open)
      (expand-open closed open)))
  #_[closed open thread-num])

(defn init-run
  "start the run by placing start node in buffers[0]"
  []
  (reset-all) ;; calls mbuff/reset-all, resets vars here and there
  (let [snode (mb/start-node)
        ibuff (mbuff/hash-of-loc (:loc (mb/start-node)) mp/nthreads)
        rcvr (mbuff/input-buffs ibuff)
        futs (create-futures mp/nthreads dpa)]
    (mbuff/put-buff rcvr [0 snode])
    futs))

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

;; TODO needs work -- rewrite as lazy-seq!!!!
(defn pextract-path
  "extract the path from the closed-locs"
  [goal-node]
  (loop [path []
         node goal-node]
    (if (nil? node)
      path
      (let [{:keys [loc parent-loc]} node]
        (if-let [next-node (mbuff/find-in-closed parent-loc)]
          (recur (conj path loc) next-node)
          (conj path loc))))))

(defn finish-up
  [doprint]
  (if-let [node (:node @incumbent)]
    (let [path (pextract-path node)]
      (when doprint
        (doseq [ln (mo/a-overlay-path path)]
          (println ln))
        (mu/print-maze-params))
      (println "Found: Path length: " (count path)))
    (println "No path found"))
  '***)

;; https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
#_(defn psearch-start
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


