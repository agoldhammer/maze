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

(def ctrl-clock (atom 0))
(def ctrl-wave-in-progress? (atom false))
(def should-terminate? (atom false))
(def path-not-found? (atom false))

#_(defmacro create-thing
  [n create-fn init-val]
  `(mapv ~create-fn (repeat ~n ~init-val)))

(defn reset-all
  "reset buffers and counters"
  []
  (mbuff/reset-all)
  (reset! ctrl-clock (atom 0))
  (reset! incumbent {:cost Integer/MAX_VALUE :node nil})
  (reset! ctrl-wave-in-progress? false)
  (reset! should-terminate? false)
  (reset! path-not-found? false)
  #_(alter-var-root #'ctrl-msgs (constantly (vec (repeatedly (* 2 mp/nthreads) #(atom [])))))
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
#_(defn next-recip
  "return the next agent to receive a control message"
  [j]
  (let [next-thread (mod (inc j) mp/nthreads)]
    (ctrl-msgs next-thread)))

;; control registers are the concatenation of input-buffs and open-qs from mbuff

(defn get-control-reg-state
  "increment clock and get the state of a control register"
  [register]
  (let [clock (mbuff/get-clock register)
        count (mbuff/get-count register)
        tmax (mbuff/get-tmax register)]
    [clock count tmax]))

;; Mattern p 167 steps 13-14
(defn make-first-ctrl-msg
  "initiate control wave with first message"
  [register0]
  (let [clock (mbuff/inc-clock register0)
        count (mbuff/get-count register0)]
    [clock count false 0]))

(defn modify-msg
  "take an input message and register and modify according to Mattern algo to produce output message"
  [input-msg register]
  (let [[time accu invalid init] input-msg
        [clock count tmax] (get-control-reg-state register)
        new-clock (max time clock)]
    (mbuff/set-clock register new-clock)
    [time (+ accu count) (or invalid (>= tmax time)) init]))

(defn run-wave
  "run the control wave"
  []
  (let [control-regs (into mbuff/input-buffs mbuff/open-qs)
        msg0 (make-first-ctrl-msg (first control-regs))
        [_ accu invalid _] (reduce modify-msg msg0 (rest control-regs))]
    [accu invalid]
    ))

(defn not-found-detector
  "detect situation in which all input-buffs and open-qs are empty,
    indicating no path found; should therefore terminate all threads, including this one"
  []
  (while (not @path-not-found?)
    (Thread/sleep 1000)
    (let [[accu invalid] (run-wave)]
      (when (and (zero? accu)
                 (not invalid))
        (reset! path-not-found? true)
        (reset! should-terminate? true)))
    #_(Thread/sleep 50))
  :nfd-terminated)

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

(defn can-terminate?
  "can this thread terminate?"
  [input]
  (if @should-terminate? 
    true
    (let [at-goal? (:node @incumbent)]
      (if at-goal?
        (let [incumb-cost (:cost @incumbent)]
          (if-let [[_ next-node] (mbuff/quickpeek input)]
            (if (< (mb/node-total-cost next-node) incumb-cost)
              false
              true)
            true))
        false))))

(defn dpa
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [input closed open]
  (while (not (can-terminate? input))
    (intake-from-buff input closed open)
    (expand-open closed open))
  (reset! should-terminate? true)
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
    (let [nfd (future-call not-found-detector)
          threads (conj futs nfd)]
      (while (not @should-terminate?)
        (Thread/sleep 2))
      (mapv future-cancel threads)
      (if @path-not-found?
        :no-path
        :ok))))



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

(defn print-maze-with-overlay
  [path plen]
  (doseq [ln (mo/a-overlay-path path)]
    (println ln))
  (println "Path length:" plen)
  (mu/print-maze-params)
  '***
  )


(defn finish-up
  [action]
  (if-let [node (:node @incumbent)]
    (let [path (pextract-path node)
          plen (count path)
          stats (mp/get-stats plen)]
      (condp = action
        :print (print-maze-with-overlay path plen)
        :noprint (do (mu/print-maze-params)
                     (println "Path length:" plen))
        :stats stats
        stats))))

(defn psearch
    "start a new || astar search; print path overlaid result if action is :print;
    return stats if action is :stats; print results without maze if action is :noprint or missing"
  ([]
   (psearch :print))
  ([action]
   #_(println "Searching maze")
   (let [status (init-run)]
     (if (= status :ok)
       (finish-up action)
       (if (= action :print)
         (println "No path found")
         nil)))))

(defn status
  []
  (let [registers (into mbuff/input-buffs mbuff/open-qs)]
    (println "counts" (mapv mbuff/get-count registers))
    (println "clocks" (mapv mbuff/get-clock registers))
    (println "tmaxes" (mapv mbuff/get-tmax registers))
    (println "should-term" @should-terminate?)
    (println "path-not" @path-not-found?)))

(comment
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
     (println "---------"))))

