(ns maze.paral
  (:require [maze.params :as mp]
            [maze.utils :as mu]
            [maze.base :as mb]
            [maze.overlay :as mo]))

;; implementing algos from Fukunga, Botea, et al.

(def incumbent-cost (atom Integer/MAX_VALUE))

(def goal-hit (atom nil))

(defn compute-recipient
  "compute recipient of Node based on hash of its .loc
    Want all nodes with same loc to be processed by same thread"
  [node]
  (mod (hash (:loc node)) mp/nthreads))

(defn create-buffers
  "create n buffers to receive nodes"
  [n]
  (mapv ref (repeat n #{})))

(defn create-counters
  "send or receive counters for termination detection"
  [n]
  (mapv ref (repeat n 0)))

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

(def buffers (create-buffers mp/nthreads))

(def counters (create-counters mp/nthreads))

(defn balance-counters
  "sum up all send or receive counters at time t for termination detection"
  []
  (dosync
   (reduce + (map deref counters))))

(defn reset-buffers
  "reset all buffers"
  []
  (alter-var-root #'buffers (constantly (create-buffers mp/nthreads))))

(defn reset-counters
  "reset all counters"
  []
  (alter-var-root #'counters (constantly (create-counters mp/nthreads))))

(defn reset-all
  "reset buffers and counters"
  []
  (swap! incumbent-cost (constantly Integer/MAX_VALUE))
  (swap! goal-hit (constantly nil))
  (reset-buffers)
  (reset-counters))

#_(defn get-buff
  "return the nth buffer"
  [n]
  {:pre [(< n mp/nthreads) (> n 0)]}
  (nth buffers n))

(defn put-buffer
  "put a node in buffer[compute-recipient(node)]"
  [node]
  (dosync
    (let [i (compute-recipient node)
         buffer (buffers i)
         counter (counters i)]
     ;; can't have side effects in sync!!!
     (alter counter inc)
     (alter buffer conj node))))

(defn put-vec-to-buffer
  "put a vector of nodes in buffers[compute-recipient(node)], update send-counters[i]"
  [nodes]
  {:pre [(vector? nodes)]}
  (dosync
   (doseq [node nodes]
     (put-buffer node))))

(defn take-buffer
  "get and remove first node from numbered buffer; return nil if buffer empty"
  [buffer-num]
  (dosync
    (let [buffer (buffers buffer-num)
          counter (counters buffer-num)
          node (first (ensure buffer))]
      (when node
        (alter buffer disj node)
        (alter counter dec))
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
  (when (not (nil? (mb/quickpeek open)))
    (let [node (mb/get-next! open)
          current-cost (mb/node-total-cost node)]
      (put-closed closed node)
      (if (mu/at-goal? (:loc node))
        (when (< current-cost @incumbent-cost)
          (swap! goal-hit (constantly node))
          (swap! incumbent-cost (constantly current-cost)))
        (let [succs (make-successor-nodes node)]
          (doseq [succ succs]
            (put-buffer succ))
          ;; TODO add succs to appropriate buffer
          )))))

(def log-agent (agent nil))

(defn log [thread-num & mesg]
  (send log-agent #(println (clojure.string/join " " (concat (str thread-num) mesg)) %)))

(defn keep-going ;; see the cited paper
  []
  ;; if at goal, continue if counter balance is positive, stop if 0
  ;; if not at goal, continue
  (if @goal-hit
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
  (while (keep-going)
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
    (put-buffer snode)))

(defn pstatus
  []
  (clojure.string/join ": " (list
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
     (println (map #(deref % 10000 :timedout) rets)))
   (let [path (pextract-path @goal-hit)]
     (if (= @incumbent-cost Integer/MAX_VALUE)
       (println "No path found")
       (if doprint 
         (do
           (doseq [ln (mo/a-overlay-path path)]
             (println ln))
           (println "Found: Path length: " (count path))
           (mu/print-maze-params))
         (println "Path length:" (count path)))))))

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