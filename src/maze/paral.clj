(ns maze.paral
  (:require [maze.params :as mp]
            [maze.utils :as mu]
            [maze.base :as mb]
            [maze.overlay :as mo]))

;; implementing algos from Fukunga, Botea, et al.

(def incumbent (atom {:node nil :cost Integer/MAX_VALUE}))

#_(def incumbent-cost (atom Integer/MAX_VALUE))

#_(def goal-hit (atom nil))

(defn compute-recipient
  "compute recipient of Node based on hash of its .loc
    Want all nodes with same loc to be processed by same thread"
  [node]
  (mod (hash (:loc node)) mp/nthreads))

(def buffers [])
(def clocks [])
(def counters [])
(def tmaxes [])

(defmacro create-thing
  [n create-fn init-val]
  `(mapv ~create-fn (repeat ~n ~init-val)))

#_(defn create-buffers
  "create n buffers to receive nodes"
  [n]
  (mapv ref (repeat n #{})))

#_(defn create-counters
  "send or receive counters for termination detection"
  [n]
  (mapv ref (repeat n 0)))

#_(defn create-clocks
  [n]
  (mapv atom (repeat n 0)))

#_(def send-counters (ref (create-counters mp/nthreads)))
#_(def recv-counters (ref (create-counters mp/nthreads)))

#_(defn inc-counter
  "inc the i-th counter of counters"
  [send-or-recv-counters i num-nodes-sent-or-rcvd]
  (dosync
    (let [count (nth @send-or-recv-counters i)]
      (alter send-or-recv-counters #(assoc % i (+ num-nodes-sent-or-rcvd count))))))

#_(defn reset-counters
    "reset both send and receive counters"
    []
    (dosync
      (ref-set send-counters (create-counters mp/nthreads))
    (ref-set recv-counters (create-counters mp/nthreads))))

(comment 
 (def buffers (create-thing mp/nthreads ref #{}))
 
 (def counters (create-thing mp/nthreads ref 0))
 
 (def clocks (create-thing mp/nthreads atom 0))
 
 (def tmaxes (create-thing mp/nthreads atom 0)))

(defn balance-counters
  "sum up all send or receive counters at time t for termination detection"
  []
  (dosync
   (map ensure counters)
   (reduce + (map deref counters))))

(comment 
 (defn reset-buffers
   "reset all buffers"
   []
   (alter-var-root #'buffers (constantly (create-buffers mp/nthreads))))
 
 (defn reset-counters
   "reset all counters"
   []
   (alter-var-root #'counters (constantly (create-counters mp/nthreads))))
 
 (defn reset-clocks
   "reset all clocks"
   []
   (alter-var-root #'clocks (constantly (create-counters mp/nthreads)))))

#_(defn reset-all
  "reset buffers and counters"
  []
  (swap! incumbent merge {:cost Integer/MAX_VALUE :node nil})
  (reset-buffers)
  (reset-counters)
  (reset-clocks))

(defn reset-all
  "reset buffers and counters"
  []
  (swap! incumbent merge {:cost Integer/MAX_VALUE :node nil})
  (doseq [[sym create-fn init] [[#'buffers ref #{}] [#'counters ref 0]
                                [#'clocks atom 0] [#'tmaxes atom 0]]]
    (alter-var-root sym (constantly (create-thing mp/nthreads create-fn init)))))

#_(defn get-buff
  "return the nth buffer"
  [n]
  {:pre [(< n mp/nthreads) (> n 0)]}
  (nth buffers n))
;; puts and takes to thread buffer are 'timestamped messages' in the sense of Mattern paper
;; A mesg is a vector [CLOCK, node]
;; this is SEND in Mattern's terminology
(defn put-buffer
  "put a node from thread 'sender' (thread-num) in buffer[compute-recipient(node)]"
  [node sender]
  (dosync
    (let [receiver (compute-recipient node)
          buffer (buffers receiver)
          counter (counters sender)
          clock (clocks sender)]
     ;; can't have side effects in sync!!!
      (swap! clock inc)
      (alter counter inc)
      (alter buffer conj [@clock node]))))

(defn put-vec-to-buffer
  "put a vector of nodes in buffers[compute-recipient(node)], update send-counters[i]"
  [nodes sender]
  {:pre [(vector? nodes)]}
  (doseq [node nodes]
    (put-buffer node sender)))

(defn take-buffer
  "get and remove first node from numbered buffer; return nil if buffer empty"
  [receiver]
  (dosync
    (let [buffer (buffers receiver)
          counter (counters receiver)
          tmax (tmaxes receiver)
          msg (first (ensure buffer))
          [tstamp node] msg ]
      (when node
        (alter buffer disj msg)
        (alter counter dec)
        (swap! tmax max tstamp))
      node)))

;; !!! This is a mutating function, might want to find another way
(defn put-open
  "add node to open (priority) queue"
  [open node]
  (mb/add-nodes! open [node]))

(defmacro setup-future
  "set up ith future"
  [i func]
  `(future (let [closed# (atom (hash-map))
                 ;; don't need atom here because priq is mutable
                 open# (mb/new-priq [] 100)
                 thread-num# ~i]
             (~func closed# open# thread-num#))))

(defn create-futures
  "create n futures from setup-thread; func(closed, open, thread-num)"
  [n func]
  (into [] (for [i (range n)] (setup-future i func)))
  )

(defn make-successor-nodes
  "make successors to the current node"
  [node]
  (let [loc (.loc node)
        newg (inc (.g node))
        succs (mu/successors loc)
        goal @mp/goal*]
    ;; using whole node as parent, not just its .loc
    (into [] (for [s succs]
               (mb/->Node s node newg (mb/calc-heuristic s goal))))))

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

(defn expand-open
  "take next node from open Frontier on thread and expand it"
  [closed open thread-num]
  (when (not (mb/deserted? open))
    (let [node (mb/get-next! open)
          current-cost (mb/node-total-cost node)]
      (put-closed closed node)
      (if (mu/at-goal? (:loc node))
        (when (< current-cost (:cost @incumbent))
          (swap! incumbent merge {:cost current-cost :node node}))
        (let [succs (make-successor-nodes node)]
          (doseq [succ succs]
            (put-buffer succ thread-num)))))))

(def log-agent (agent nil))

(defn log [thread-num & mesg]
  (send log-agent #(println (clojure.string/join " " (concat (str thread-num) mesg)) %)))

;; for details of termination algorithm, see Mattern 1987 paper, section 6, p. 166
(defn keep-going? 
  []
  ;; if at goal, continue if counter balance is positive, stop if 0
  ;; if not at goal, continue
  (if (:node @incumbent)
    (pos? (balance-counters))
    true))

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
          (put-open open n')))
      (do 
        #_(log thread-num "Putting" n')
        (put-open open n'))))
  [closed open thread-num])

(defn dpa
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [closed open thread-num]
  (while (keep-going?)
    (intake-from-buff closed open thread-num)
    (expand-open closed open thread-num))
  :terminated
  )

;; dpa development version, using dotimes instead of while
(defn xdpa
  [closed open thread-num]
  (dotimes [t 1]
    (log thread-num "starting")
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

(defn pstatus
  []
  (clojure.string/join "\n" (list
                             (str "Buffers: " (mapv deref buffers))
                             (str "counters: " (mapv deref counters)))))

(defn pextract-path
  [goal-node]
  (loop [path []
         node goal-node]
    (if (nil? node)
      path
      (recur (conj path (:loc node)) (:parent node)))))

;; https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
(defn psearch-start
  "start a new || astar search; print path overlaid result if doprint is true"
  ([]
   (psearch-start true))
  ([doprint]
   (reset-all)
   (init-run)
   (println "Searching maze")
   (let [rets (create-futures mp/nthreads dpa)]
     (println (map #(deref % 5000 :timedout) rets))
     (map future-cancel rets))
   (let [path (pextract-path (:node @incumbent))]
     (if (= {:cost @incumbent} Integer/MAX_VALUE)
       (println "No path found")
       (if doprint 
         (do
           (doseq [ln (mo/a-overlay-path path)]
             (println ln))
           (println "Found: Path length: " (count path))
           (mu/print-maze-params))
         (println "Path length:" (count path)))))))
