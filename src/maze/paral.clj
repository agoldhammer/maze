(ns maze.paral
  (:require [maze.params :as mp]
            [maze.utils :as mu]
            [maze.base :as mb]))

;; implementing algos from Fukunga, Botea, et al.

(defn compute-recipient
  "compute recipient of Node based on has of its .loc"
  [node]
  (mod (hash node) mp/nthreads))

(defn create-buffers
  "create n buffers to receive nodes"
  [n]
  (into [] (repeatedly n (partial ref #{}))))

(defn create-counters
  "send or receive counters for termination detection"
  [n]
  (atom 
   (into [] (repeat n 0))))

(def send-counters (create-counters mp/nthreads))
(def recv-counters (create-counters mp/nthreads))

(defn inc-counter
  "inc the i-th counter of counters"
  [send-or-recv-counters i num-nodes-sent-or-rcvd]
  (swap! send-or-recv-counters #(assoc % i (+ num-nodes-sent-or-rcvd (nth % i)))))

(defn sum-counters
  "sum up all send or receive counters at time t for termination detection"
  [send-or-recv-counters]
  (reduce +  @send-or-recv-counters))

(defn reset-counters
  "reset both send and receive counters"
  []
  (let [counters [send-counters recv-counters]]
    (doseq [counter counters]
      (reset! counter @(create-counters mp/nthreads)))))

(def buffers (create-buffers mp/nthreads))

(defn reset-buffers
  "reset all buffers"
  []
  (alter-var-root #'buffers (constantly (create-buffers mp/nthreads))))

(defn reset-all
  "reset buffers and counters"
  []
  (reset-buffers)
  (reset-counters))

#_(defn get-buff
  "return the nth buffer"
  [n]
  {:pre [(< n mp/nthreads) (> n 0)]}
  (nth buffers n))

(defn put-buffer
  "put a node or nodes in buffers[compute-recipient(node)], update send-counters[i]"
  [node-or-nodes]
  (let [is-vec? (vector? node-or-nodes)
        nodes (if is-vec? node-or-nodes (vec node-or-nodes))]
    (dosync
      (doseq [node nodes]
        (let [i (compute-recipient node)]
          (inc-counter send-counters i 1)
          (alter (nth buffers i) conj node))))))

(defn buffer->vec-of-nodes!
  "if buffer = buffers[i] not empty, return contents as vector and reset; else return empty vec
   buffer must be a ref; update recv-counters[i] (e.g. created by create-buffers)"
  [i]
  (dosync
    (let [buffer (nth buffers i)
          nodes (seq (ensure buffer))
          nodevec (into [] nodes)
          cnt (count nodevec)]
      (inc-counter recv-counters i cnt)
      (when nodes
        (ref-set buffer #{}))
      nodevec)))

(defn add-to-open-queue!
  "add nodes to open queue"
  [open node-or-nodes]
  (if (vector? node-or-nodes)
    (mb/add-nodes! open node-or-nodes)
    (mb/add-nodes! open [node-or-nodes])))

(defmacro setup-thread
  "set up locals for ith thread; func shd be f(closed open thread-num)"
  [i func]
  `(let [closed# (atom (hash-map))
         open# (atom (mb/new-priq [] 100))
         thread-num# ~i]
     (~func closed# open# thread-num#)))

(defn create-futures
  "create n futures from setup-thread; func(closed, open, thread-num)"
  [n func]
  ((let [fs (into [] (for [i (range n)] (setup-thread i func)))]
     (map #(future %) fs))))

#_(defn create-expanders
  "helper for thread functions to expand nodes"
  [n start-node]
  (let [thread-params (vec (repeatedly mp/nthreads create-thread-params))
        start-recipient (compute-recipient start-node)]
    (add-to-open-queue! (nth thread-params start-recipient) start-node)
    thread-params))

(def terminate-flag (atom false))

(def incumbent-cost (atom Integer/MAX_VALUE))

(defn make-successor-nodes
  "make successors to the current node"
  [node]
  (let [loc (.loc node)
       newg (inc (.g node))
       succs (mu/successors (.loc node))
       goal @mp/goal*]
   (into [] (for [s succs]
              (mb/->Node s loc newg (mb/calc-heuristic s goal))))
   ))

(defn put-closed
  "add node to closed buffer if loc not present or cost of new < cost old"
  [closed node]
  (let [cl @closed
        loc (:loc node)
        oldn (get cl loc)]
    (if (or (nil? oldn) (< node oldn))
      (swap! closed assoc loc node))
    ))

(defmacro get-from-closed
  "is node in closed?"
  [closed node]
  `(get @~closed (.loc ~node)))

(defn expand-open
  "take next node from open Frontier on thread and expand it"
  [open closed]
  (if-let [testnode (mb/quickpeek open)]
    (let [current-cost (mb/node-total-cost testnode)]
      (if (> current-cost @incumbent-cost)
        nil
        (let [node (mb/get-next! open)
              succs (make-successor-nodes node)]
          (put-closed closed node)
          (put-buffer succs)
          ;; TODO add succs to appropriate buffer
          )))))

(defn terminate-detect
  ;; see the cited paper
  []
  ;; decrement rcvd count to allow for initial message
  (let [sent (sum-counters send-counters)
        rcvd (dec (sum-counters recv-counters))]
    (not (and (> rcvd 0)
              (= rcvd sent)))))

(defn dpa
  "distributed parallel astar algo
    this fn is to be fed to create thread bodies"
  [closed open thread-num]
  (while (terminate-detect)
    (let [nodes (buffer->vec-of-nodes! (buffers thread-num))]
      (when nodes
        (println nodes))))
  )

#_(def dpa
    "distributed parallel astar algo"
    (with-local-vars [open (mb/new-priq [] 1000)
                    closed (hash-map)
                    nbuff 0 ;; TODO need to set this properly when creating thread
                    buffer (get-buff nbuff)]
    (while (terminate-detect)
      (let [nodes (buffer->vec-of-nodes! buffer)]
        (doseq [node nodes]
          (when-let [n' (get-from-closed closed node)]
            ()))
        ))
    
    ))

#_(defn make-threads
  "make threads to run dpa algo"
  []
  (let [bodies (create-thread-bodies mp/nthreads dpa)
        results (into [] (map #(future %) bodies))]
    results))

(defn start-run
  "start the run by placing start node in buffers[0]"
  []
  (let [snode (mb/start-node)]
    (dosync
     (alter (buffers 0) conj snode))) )

;; https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
#_(defn psearch-start
  "start a new || astar search; print path overlaid result if doprint is true"
  ([]
   (psearch-start true))
  ([doprint]
   (send max-frontier-size (constantly 0) 0)
   (let [start-node (mc/astar-start)
         buffers (create-buffers nthreads)]
     (while (keep-going?)
       )
     #_(let [{:keys [found]} (par-astar-search-maze start-node @goal* false)]
         (if found
           (do
             (let [path (mc/extract-path)]
               (when doprint
                 (doseq [ln (mc/a-overlay-path path)]
                   (println ln)))
               (println "Found: Path length: " (count path))
               (mc/print-maze-params)))
           (println "No path was found"))))))



#_(defn par-astar-search-maze
  "start-frontier is a priority queue of Node types"
  [start-frontier goal is-subsearch?]
  (when (not is-subsearch?) (println "Starting parallel search"))
  (loop [frontier start-frontier]
    (send max-frontier-size max (mb/countf frontier))
    (if (and (not is-subsearch?)
             (>= (countf frontier) split-frontier-at))
      (let [new-frontiers (split-frontier frontier nthreads)
            ;; _ (println "new frons" new-frontiers)
            ;; _ (println "counts" (map countf new-frontiers))
            sub-results (pmap #(par-astar-search-maze % goal true) new-frontiers)
            ;; _ (doseq [i (range (count new-frontiers))]
             ;;   (println "ith front" i (par-astar-search-maze (nth new-frontiers (inc i)) goal true) "done"))
            
            ]
        (println "subresults" sub-results)
        (if (every? not (map :found sub-results))
          {:found false}
          {:found true}))
      ;; else
      (if (deserted? frontier)
        {:found false};
        ; else
        (let [working-node (get-next! frontier)
              loc (.loc working-node)
              parent (.parent working-node)]
          (if (at-goal? loc)
            (dosync
             (alter a-visited* assoc loc {:cost nil :parent parent})
             {:found true})
            ; else, want to expand nod if new or cheaper than on prev visit to this loc
            (let [action (dosync
                          (let [current-f (node-total-cost working-node)
                                old-f (:cost (get (ensure a-visited*) loc nil))
                                state (cond
                                        (nil? old-f) :new
                                        (< current-f old-f) :cheaper
                                        :else :old)]
                            (when (= state :new)
                              (alter a-visited* assoc loc
                                     {:cost current-f :parent parent}))
                            (when (= state :cheaper)
                              (alter a-visited* dissoc loc))
                            #_(println "new or ch" new-or-cheaper?)
                            state))]
              (when (= action :cheaper) ;; need to redo working-node with new cheaper cost
                (add-nodes! frontier [working-node]))
              (when (= action :new)  ;; add in children if new or ceaper
                (let [new-g (inc (.g working-node))
                      unvisited (unvisited-cheaper-successors loc goal new-g)]
                  #_(println "unvisited" (count unvisited))
                  (add-nodes! frontier unvisited))
                #_(println "frontier" frontier))
              
              (recur frontier))))))))

#_(defn par-start-astar-search
  "start a new astar search; print path overlaid result if doprint is true"
  ([]
   (par-start-astar-search true))
  ([doprint]
   (dosync (ref-set mp/a-visited* (hash-map)))
   (send max-frontier-size (constantly 0) 0)
   (let [start-node (mc/astar-start)]
     (let [{:keys [found]} (par-astar-search-maze start-node @goal* false)]
       (if found
         (do
           (let [path (mc/extract-path)]
             (when doprint
               (doseq [ln (mc/a-overlay-path path)]
                 (println ln)))
             (println "Found: Path length: " (count path))
             (mc/print-maze-params)))
         (println "No path was found"))))))