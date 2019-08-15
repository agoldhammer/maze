(ns maze.paral
  (:require [maze.params :refer [a-visited* goal* max-frontier-size
                                 nthreads split-frontier-at]]
            [maze.core :as mc :refer [node-total-cost unvisited-cheaper-successors
                                      add-nodes get-next at-goal? countf deserted?
                                      split-frontier]]))

(defn par-astar-search-maze
  "start-frontier is a priority queue of Node types"
  [start-frontier goal is-subsearch?]
  (when (not is-subsearch?) (println "Starting parallel search"))
  (loop [frontier start-frontier]
    (send max-frontier-size max (countf frontier))
    (if (and (not is-subsearch?)
             (>= (countf frontier) split-frontier-at))
      (let [sub-results 
            (pmap #(par-astar-search-maze % goal true) (split-frontier frontier nthreads))]
        (println "subresults" sub-results)
        (if (some :found sub-results)
          {:found true}
          {:found false}))
      ;; else
      (if (deserted? frontier)
        {:found false};
        ; else
        (let [working-node (get-next frontier)
              loc (.loc working-node)
              parent (.parent working-node)]
          (if (at-goal? loc)
            (dosync
             (alter a-visited* assoc loc {:cost nil :parent parent})
             {:found true})
            ; else, want to expand nod if new or cheaper than on prev visit to this loc
            (let [should-expand? (dosync
                                  (let [current-f (node-total-cost working-node)
                                        old-f (:cost (get (ensure a-visited*) loc nil))
                                        new-or-cheaper? (or (nil? old-f)
                                                            (< current-f old-f))]
                                    (when new-or-cheaper?
                                      (alter a-visited* assoc loc
                                             {:cost current-f :parent parent}))
                                    #_(println "new or ch" new-or-cheaper?)
                                    new-or-cheaper?))]
              
              (when should-expand?
                (let [new-g (inc (.g working-node))
                      unvisited (unvisited-cheaper-successors loc goal new-g)]
                  #_(println "unvisited" (count unvisited))
                  (add-nodes frontier unvisited))
                #_(println "frontier" frontier))
              (recur frontier))))))))

(defn par-start-astar-search
  "start a new astar search; print path overlaid result if doprint is true"
  ([]
   (par-start-astar-search true))
  ([doprint]
   (dosync (ref-set a-visited* (hash-map)))
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