(ns maze.core
  (:require [taoensso.nippy :as nippy]
            [maze.params :as mp]
            [maze.base :as mb]
            [maze.overlay :as mo]
            [maze.utils :as mu :refer [make-maze]]
            [maze.paral :as mpar]
            [clojure.set]
            #_[criterium.core :as cr])
  (:gen-class))

(comment 
 (def DEBUG 0)
 
 (defmacro on-debug [& body]
   `(when (> DEBUG 1)
      (do ~@body)))
 
 (defmacro dbg [x]
   `(when (> DEBUG 0)
      (println ~(format "%s:%s> is"
                        *file*
                        (:line (meta &form)))
               ~x))))

#_(def pq (priority-queue 100 node-comp))

;; -----------------------------------------

(defn bfs-start
  "return initial frontier (Fifo) for breadth first search"
  []
  (mb/->Fifo (mb/init-frontier)))

(defn dfs-start
  "return initial frontier (Stack) for depth first search"
  []
  (mb/->StackD (mb/init-frontier)))

; a node is a map with components loc and parent
(defn loc->node
  "create a node from a loc by adding parent"
  [loc path]
  {:loc loc :path (conj path loc)})

;; TODO start should initialize the frontier rather than initializing in loop
(defn search-maze
  "type of search is determined by type of frontier passed in,
    which may be Stack, Fifo, or PriorityQueue"
  [start]
  (loop [frontier start]
    #_(println "Frontier length: " (countf frontier))
    (if (not (mb/deserted? frontier))
      (let [working-node (mb/get-next frontier)
            {:keys [loc path]} working-node]
        (if (not (mu/visited? loc))
          (do (swap! mp/visited* conj loc)
              (cond
                (nil? loc) {:found false}
                (mu/at-goal? loc) {:found true :path path}
                :else (let [succ (mu/successors loc)
                            unvisited (filter #(not (mu/visited? %)) succ)
                            nodes (mapv #(loc->node % path) unvisited)
                            new-frontier (mb/add-nodes! frontier nodes)]
                        (recur new-frontier))))
          (recur (mb/remainder frontier))))
      {:found false})))

(defn filter-function-factory
  "return a function to pass a node
   if loc is unvisited or if its total-cost is less than old cost;
   ensure consistency of filtering of all child nodes for given node
   by dereference a-visited* only once for each parent"
  []
  (let [visited @mp/a-visited*]
    (fn 
      [node]
      (let [loc (.loc node)
            new-f (mb/node-total-cost node)
            ;; this doesn't need to be synced b/c cost can only decrease
            old-f (:cost (get visited loc))]
        (or (nil? old-f)
            (< new-f old-f))))))

(defn unvisited-cheaper-successors
  "find unvisited cheaper successors;
   returns as seq of Nodes"
  [loc goal g-of-new-node]
  (let [succ (mu/successors loc)
        nodes (map #(mb/loc->Node %1 loc goal g-of-new-node) succ)]
    (filter (filter-function-factory) nodes)))

(defn astar-search-maze
  "start-frontier is a priority queue of Node types"
  [start-frontier goal]
  (loop [frontier start-frontier]
    (send mp/max-frontier-size max (mb/countf frontier))
    #_(println "Frontier length: " (countf frontier))
    (if (mb/deserted? frontier)
      {:found false};
      ; else
      (let [working-node (mb/get-next! frontier)
            loc (.loc working-node)
            parent (.parent-loc working-node)]
        (if (mu/at-goal? loc)
          (dosync
           (alter mp/a-visited* assoc loc {:cost nil :parent parent})
           {:found true})
          ; else, want to expand nod if new or cheaper than on prev visit to this loc
          (let [should-expand? (dosync
                                (let [current-f (mb/node-total-cost working-node)
                                      old-f (:cost (get (ensure mp/a-visited*) loc nil))
                                      new-or-cheaper? (or (nil? old-f)
                                                          (< current-f old-f))]
                                  (when new-or-cheaper?
                                    (alter mp/a-visited* assoc loc
                                           {:cost current-f :parent parent}))
                                  #_(println "new or ch" new-or-cheaper?)
                                  new-or-cheaper?))]

            (when should-expand?
              (let [new-g (inc (.g working-node))
                    unvisited (unvisited-cheaper-successors loc goal new-g)]
                #_(println "unvisited" (count unvisited))
                (mb/add-nodes! frontier unvisited))
              #_(println "frontier" frontier))
            (recur frontier)))))))

(defn start-search
  "start a new search; print path overlaid result if doprint is true"
  ([]
   (start-search :bfs true))
  ([type-of-search doprint]
   (reset! mp/visited* #{})
   ;; TODO fix this to initialize for other types of searches
   (let [init-fn (if (= type-of-search :bfs) bfs-start dfs-start)
         start (init-fn)]
     (let [{:keys [found path]} (search-maze start)]
       (if found
         (do
           (when doprint
             (doseq [ln (mo/overlay-path (drop-last 1 path))]
               (println ln)))
           (println "Found: Path length: " (count path))
           (mu/print-maze-params))
         (println "No path was found"))))))

(defn print-maze
  [path]
  (doseq [ln (mo/a-overlay-path path)]
    (println ln))
  (println "Found: Path length: " (count path))
  (mu/print-maze-params)
  '***)

(defn astar
  "start a new astar search; print path overlaid result if doprint is true"
  ([]
   (astar :print))
  ([action]
   (dosync (ref-set mp/a-visited* (hash-map)))
   (send mp/max-frontier-size (constantly 0) 0)
   (let [start-frontier (mb/astar-start-frontier)]
     (let [{:keys [found]} (astar-search-maze start-frontier mp/goal*)]
       (if found
         (let [path (mo/extract-path)
               plen (count path)
               stats (mp/get-stats plen)]
           (condp = action
             :print (print-maze path)
             :stats  stats
             :noprint (do (mu/print-maze-params)
                          (println "Path length:" plen))
             stats))
         (when (= action :print)
           (println "No path was found")))))))
     
(defn new-maze-problem
  [size sparsity]
  (mu/make-maze size sparsity)
  (start-search)
  "Done with problem"
  #_(start-bfs-search)
  #_(start-dfs-search))

(defn get-proj-dir
  []
  (str (System/getProperty "user.dir") "/"))

(defn save-maze
  [fname]
  (let [f (clojure.java.io/file (str (get-proj-dir) fname ".maz"))]
    (nippy/freeze-to-file f {:start mp/start*
                             :goal mp/goal*
                             :size mp/size*
                             :maze mp/maze*}))
  (println "Maze saved as" fname))

(defn read-maze
  [fname]
  (let [f (clojure.java.io/file (str (get-proj-dir) fname ".maz"))
        parms (nippy/thaw-from-file f)]
    (alter-var-root #'mp/start* (constantly (:start parms)))
    (alter-var-root #'mp/goal* (constantly (:goal parms)))
    (alter-var-root #'mp/size* (constantly (:size parms)))
    (alter-var-root #'mp/maze* (constantly (:maze parms))))
  (println "Read maze" fname))

(defn pstar
  "do parallel astar search, print if doprint true"
  ([]
   (pstar :print))
  ([action]
   (mpar/psearch action)))

(defn compstar
  "compare astar and pstar"
  []
  (mu/speedup pstar astar :stats))

(defn avg-speedup
  "average n runs of compstar and extract relevant data"
  [n]
  (let [runs (map mu/extract-data (take n (repeatedly compstar)))
        avg-time (/ (reduce + (map #(:time %) runs)) n)
        avg-speedup (/ (reduce + (map #(:speedup %) runs)) n)
        run (first runs)]
    {:path (:path run) :size (:size run) :avg-time avg-time :avg-speedup avg-speedup}))

(defn compile-avg-stats
  "compile statistics on mazes of varying size, repeating/averaging measurement ntimes on each"
  [ntimes start stop step sparsity]
  (for [size (range start stop step)]
    (do
      (make-maze size sparsity false)
      #_(mu/ticks pstar :stats)
      (avg-speedup ntimes))))

(defn run-stat
  "compile statistics on mazes of varying size, repeating measurement ntimes on each"
  [ntimes size sparsity]
  (do
    (make-maze size sparsity false)
    (doall (take ntimes (repeatedly compstar)))))

(defn correct-keys
  "correct keys in stats map and handle case of no path found"
  [stat]
  (let [corrected (clojure.set/rename-keys stat {:time1 :time})]
    (if-not (get corrected :path)
      (merge corrected {:path 0 :size 0 :sparsity 0})
      corrected)))

(defn compile-stats
  "compile statistics on mazes of varying size, repeating measurement ntimes on each"
  [ntimes start stop step sparsity]
  (let [f (fn [stat] (dissoc (merge stat (:stats1 stat)) :stats1 :stats2))]
    (doall (map correct-keys
                (doall (map f
                            (apply concat (for [size (range start stop step)]
                                            (let [res 
                                                  (run-stat ntimes size sparsity)]
                                              res)))))))))

(defn pstar-stat
  "get stats on pstar for maze of size and sparsity"
  [size sparsity]
  (let [f (fn [stat] (dissoc (merge stat (:result stat)) :result))
        _ (make-maze size sparsity false)
        res (f (mu/ticks pstar false))]
    ; handle case where there is no path
    (if-not (nil? (:path res))
      res
      (merge res {:path 0 :size size :sparsity sparsity}))))

(defn pstar-stats
  "compile statistics using pstar only, ntimes examples at each size"
  [ntimes start stop step sparsity]
  (apply concat 
    (for [size (range start stop step)]
      (take ntimes (repeatedly #(pstar-stat size sparsity))))))

;; USAGE: (to-edn "stats.edn" compile-stats/compile-avg-stats ntimes start stop step sparsity)
(defn to-edn
  "save stats to edn file"
  [fname f & args]
  (spit fname (pr-str (apply f args))))

(defn from-edn
  "read from edn file"
  [fname]
  (clojure.edn/read-string (slurp fname)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (make-full-maze 50 30)
  (start-search) ; bfs
  (start-search :dfs true)
  (astar :print)
  (astar :stats)
  (pstar :noprint)
  ;; to compile stats on pstar runs, 2 at each size, in size range 40 to 70 by 10s, sparsity 30
  (to-edn "stats.edn" pstar-stats 2 40 80 10 30)
 ; params to search can be :print, :noprint, or :stats; default is :print
  (compstar) ;compare times of pstar and astar
  ) ; dfs

 

