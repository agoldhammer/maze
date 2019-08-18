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
  (into [] (repeatedly 4 (partial ref #{}))))

(defn put-buffer
  "put a node in buffer"
  [buffer node]
  (dosync (alter buffer conj node)))

(defn buffer-next
  "take next node from buffer, delete and return it if there is one; return nil if not
   buffer must be a ref (e.g. created by create-buffers)"
  [buff]
  (dosync
    (let [nodes (seq (ensure buff))]
      (when nodes
        (alter buff disj (first nodes)))
      (first nodes))))

(defn create-thread-params
  "create the open and closed structures for a thread, w or w/o init"
  []
  (let [thr 
        {:open (mb/new-priq [] 1000)
         :closed (hash-map)}]
    thr))

(defn get-open
  "return the open queue from threat params tp"
  [tp]
  (:open tp))

(defn add-to-open-queue
  "add nodes to open queue"
  [thread-params node-or-nodes]
  (let [pq (get-open thread-params)]
    (if (vector? node-or-nodes)
      (mb/add-nodes! pq node-or-nodes)
      (mb/add-nodes! pq [node-or-nodes]))))

(defn create-expanders
  "helper for thread functions to expand nodes"
  [n start-node]
  (let [thread-params (vec (repeatedly mp/nthreads create-thread-params))
        start-recipient (compute-recipient start-node)]
    (add-to-open-queue (nth thread-params start-recipient) start-node)
    thread-params))

(def terminate-flag (atom false))

(defn keep-going?
  []
  (not @terminate-flag))

(def incumbent-cost (atom Integer/MAX_VALUE))

(defn add-to-closed
  "add node to closed set"
  [node closed]
  (conj closed node))

(defn make-successor-nodes
 "make successors to the current node"
 [node]
 (let [loc (.loc node)
       newg (inc (.g node))
       succs (mu/successors (.loc node))
       goal @mp/goal*]
   (into [] (for [s succs]
              (mb/->Node s loc newg (mu/calc-heuristic s goal))))
   ))

(defn expand-open
  "take next node from open Frontier on thread and expand it"
  [open closed]
  (if-let [testnode (mb/quickpeek open)]
    (let [current-cost (mb/node-total-cost testnode)]
      (if (> current-cost @incumbent-cost)
        nil
        (let [node (mb/get-next! open)
              succ (make-successor-nodes node)]
          (add-to-closed closed node)
          (mb/add-nodes! open succ)) ;; fix this, must return val or use mutable closed
        ))))


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