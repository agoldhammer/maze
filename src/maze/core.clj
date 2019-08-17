(ns maze.core
  (:require [taoensso.nippy :as nippy]
            [maze.params :as mp])
  (:import [java.util.concurrent PriorityBlockingQueue])
  (:gen-class))

(def DEBUG 0)

(defmacro on-debug [& body]
  `(when (> DEBUG 1)
     (do ~@body)))

(defmacro dbg [x]
  `(when (> DEBUG 0)
     (println ~(format "%s:%s> is"
                       *file*
                       (:line (meta &form)))
              ~x)))

(declare print-maze)


(defn init-frontier
  "return a frontier with 1 node: loc start and path at start"
  []
  (let [start-loc @mp/start*]
    [{:loc start-loc :path [start-loc]}]))

(defn priority-queue
  ([]
   (PriorityBlockingQueue.))
  ([^java.util.Collection xs]
   (PriorityBlockingQueue. xs))
  ([init-size ordering]
   (PriorityBlockingQueue. init-size (comparator ordering))))

;; Node type has field loc (location), parent, g (cost to node), and
;; h (heuristic); total cost f = g + h
(deftype Node [loc parent g h])

(defn node->tuple
  "returns [loc parent g h] from Node"
  [node]
  [(.loc node) (.parent node) (.g node) (.h node)])

(defn node-total-cost [^Node node]
  (+ (.g node) (.h node)))

(defn node-comp [^Node n1 ^Node n2]
  (< (node-total-cost n1) (node-total-cost n2)))

(defn calc-heuristic
  "given two locs, loca and locb, calculate the Manhattan distance"
  [loca locb]
  (let [[xdist ydist] (mapv - loca locb)]
    (+ (Math/abs xdist) (Math/abs ydist))))

#_(def pq (priority-queue 100 node-comp))

;; The Frontier protocol expects this to be a type containing a sequence of nodes
;; for use by the search algorithm
;; Three concrete types are implemented: a Fifo, a Stack, and a Priority queue
;; A node is a map containing a :loc, a :path to that :loc, and optionally
;; a :heuristic for use by the A* algo

(defprotocol Frontier
  "protocol for handling various frontier types"
  (countf [this] "return count of node list")
  (get-next [this] "get next node")
  (raw-remainder [this] "return the underlying remainder node sequence without converting to type")
  (remainder [this] "remainder (as type) after dropping the next node")
  (add-nodes! [this vec-of-nodes] "add nodes in vector and return new frontier")
  (deserted? [this] "Is the frontier empty?")
  )

(deftype Fifo [nodes])

(extend-protocol Frontier
  Fifo
  (countf [this]
    (count (.nodes this)))
  (get-next [this]
    (nth (.nodes this) 0))
  (raw-remainder [this]
    (subvec (.nodes this) 1))
  (remainder [this]
    (->Fifo (raw-remainder this)))
  (add-nodes! [this v-of-nodes]
    (->Fifo (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))

(declare new-priq)

(defn split-frontier
  "divide frontier into n or n+1 sub-frontiers"
  [frontier n]
  {:pre [(> n 0)]}
  ;; randomize assignment to sub-frontiers by shuffling
  (let [size (quot (countf frontier) n)
        vec-of-nodes (shuffle (into [] (.toArray (.pq frontier))))]
    (doall 
     (map #(new-priq %1 1000) (partition-all size vec-of-nodes))))
  )

;; TODO ------------------------------------
(deftype StackD [nodes])

(extend-protocol Frontier
  StackD
  (countf [this] (count (.nodes this)))
  (get-next [this] (peek (.nodes this)))
  (raw-remainder [this]
    (pop (.nodes this)))
  (remainder [this]
    (->StackD (raw-remainder this)))
  (add-nodes! [this v-of-nodes]
    (->StackD (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))


(deftype PriQ [pq])

(extend-protocol Frontier
  PriQ
  (countf [this]
    (.size (.pq this)))
  (add-nodes! [this v-of-nodes]
             (doseq [v v-of-nodes]
               (.add (.pq this) v)))
  ;; note!! remainder is not used, get next strips head of queue
  (get-next [this]
    (.take (.pq this)))
  (remainder [this]
    (.take (.pq this))
    this)
  (deserted? [this]
    (nil? (.peek (.pq this)))))

;; -----------------------------------------

(defn bfs-start
  "return initial frontier (Fifo) for breadth first search"
  []
  (->Fifo (init-frontier)))

(defn dfs-start
  "return initial frontier (Stack) for depth first search"
  []
  (->StackD (init-frontier)))

(defn new-priq
  "make a new priority queue from a vector of Nodes"
  [vec-of-Nodes size]
  (let [queue (priority-queue size node-comp)
        pq (->PriQ queue)]
    (add-nodes! pq vec-of-Nodes)
    pq))

(defn start-Node
  "return start Node"
  []
  (let [start @mp/start*]
    (->Node start nil 0 (calc-heuristic start @mp/goal*))))

(defn astar-start
  "return initial frontier (PriQ) for astar"
  []
  (let [start @mp/start*]
    (new-priq [(->Node start nil 0 (calc-heuristic start @mp/goal*))] 1000)))

(defn check-coord
  [loc limit]
  (if 
   (or (< loc 0)
       (> loc limit))
    :err
    loc))

(defn change-randomly
  "place wall elements with uniform sparsity on scale of 100"
  [sparsity]
  (fn [elt]
    (if (< (rand-int 100) sparsity)
      "x"
      ".")))

(defn put-wall-in-row
  [len sparsity]
  (let [f (change-randomly sparsity)]
    (mapv f (range len))))

(defn make-maze
  "make a bare maze of given size with wall elements of sparsity 0-99"
  [size sparsity]
  (vec (repeatedly size #(put-wall-in-row size sparsity))))

; a loc is a vec pair [row col]
(defn random-loc
  "return a random location in maze of given size"
  [size]
  ((juxt rand-int rand-int) size))

(defn update-maze
  "update maze at loc with given elt"
  [maze loc elt]
  (update-in maze loc (constantly elt)))

(defn choose-start-finish
  "choose start and finish locs randomly, making sure they are not equal"
  [size]
  (let [start (random-loc size)
        finish (repeatedly #(random-loc size))]
 
    (let [new-finish
          (first (drop-while #(= start %) finish))]
      [start new-finish])))

(defn make-full-maze
  "make maze of given size and sparsity with S and G
   indicating start and goal positions; print if doprint is true"
  ([size sparsity]
   (make-full-maze size sparsity true))
  ([size sparsity doprint]
   (let [base (make-maze size sparsity)
         [start goal] (choose-start-finish size)]
     (reset! mp/start* start)
     (reset! mp/goal*  goal)
     (reset! mp/size* size)
     (reset! mp/sparsity* sparsity)
     (reset! mp/maze* 
             (->
              base
              (update-maze start "S")
              (update-maze goal "G"))))
   (when doprint
     (print-maze))))

(defn print-maze-params
  "print just maze parameters, not maze itself"
  []
  (println "Size:" @mp/size* " Sparsity:" @mp/sparsity*)
  (println "Start:" @mp/start*)
  (println "Goal:" @mp/goal*)
  (println "Max frontier size:" @mp/max-frontier-size))

(defn print-maze
  "if doall, print maze with params; else just print params"
  ([] (print-maze true))
  ([doall]
   (when doall
     (doseq [ln @mp/maze*]
       (println ln)))
   (print-maze-params)))

(defn in-bounds?
  "check coord in bounds for size"
  [w size]
  (if (and (>= w 0) (< w size))
    w
    false))

(defn not-blocked?
  "is loc blocked?"
  [loc]
  (not= "x" 
     (get-in @mp/maze* loc)))

(defn successors
  "return set of successors to loc"
  [loc]
  (let [[x y] loc
        size @mp/size*
        x- (dec x)
        x+ (inc x)
        y- (dec y)
        y+ (inc y)
        s1 (for [xn (filter #(in-bounds? % size) (list x- x+))] [xn y])
        s2 (for [yn (filter #(in-bounds? % size) (list y- y+))] [x yn])]
    (apply vector (filter not-blocked? (into s1 s2))))
  )

(defn visited?
  "has loc been visited?"
  [loc]
  (contains? @mp/visited* loc))

(defn at-goal?
  [loc]
  (= loc @mp/goal*))

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
    (if (not (deserted? frontier))
      (let [working-node (get-next frontier)
            {:keys [loc path]} working-node]
        (if (not (visited? loc))
          (do (swap! mp/visited* conj loc)
              (cond
                (nil? loc) {:found false}
                (at-goal? loc) {:found true :path path}
                :else (let [succ (successors loc)
                            unvisited (filter #(not (visited? %)) succ)
                            nodes (mapv #(loc->node % path) unvisited)
                            new-frontier (add-nodes! frontier nodes)]
                        (recur new-frontier))))
          (recur (remainder frontier))))
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
            new-f (node-total-cost node)
            ;; this doesn't need to be synced b/c cost can only decrease
            old-f (:cost (get visited loc))]
        (or (nil? old-f)
            (< new-f old-f))))))

(defn loc->Node
  "from loc, path, goal make N Node"
  [loc parent goal g-of-new-node]
  (let [heuristic (calc-heuristic loc goal)]
    (->Node loc parent g-of-new-node heuristic)))

(defn unvisited-cheaper-successors
  "find unvisited cheaper successors;
   returns as seq of Nodes"
  [loc goal g-of-new-node]
  (let [succ (successors loc)
        nodes (map #(loc->Node %1 loc goal g-of-new-node) succ)]
    (filter (filter-function-factory) nodes)))

(defn astar-search-maze
  "start-frontier is a priority queue of Node types"
  [start-frontier goal]
  (loop [frontier start-frontier]
    (send mp/max-frontier-size max (countf frontier))
    #_(println "Frontier length: " (countf frontier))
    (if (deserted? frontier)
      {:found false};
      ; else
      (let [working-node (get-next frontier)
            loc (.loc working-node)
            parent (.parent working-node)]
        (if (at-goal? loc)
          (dosync
           (alter mp/a-visited* assoc loc {:cost nil :parent parent})
           {:found true})
          ; else, want to expand nod if new or cheaper than on prev visit to this loc
          (let [should-expand? (dosync
                                (let [current-f (node-total-cost working-node)
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
                (add-nodes! frontier unvisited))
              #_(println "frontier" frontier))
            (recur frontier)))))))

(defn overlay-path
  "print maze with search path overlay"
  [path]
  ;; drop the starting node
  (let [reduced-path (drop 1 path)
        maze @mp/maze*
        index (map #(mod % 10) (range (count reduced-path)))
        indexed-path (partition 2 (interleave index reduced-path))]
    (reduce #(update-maze %1 (second %2) (str (first %2))) maze indexed-path)))

(defn extract-path
  "extract path from a-visited; includes start and goal points
   extracted path is in reverse order, from goal to start"
  []
  ;; drop the starting node
  (let [a-visited @mp/a-visited*]
    (loop [path [@mp/goal*]]
      (let [loc (:parent (get a-visited (peek path)))]
        (if (nil? loc)
          path
          (do 
            (recur (conj path loc))))))))

(defn a-overlay-path
  "overlay the extracted path on the maze"
  [path]
  ; drop the start and goal points and reverse the path
  (let [reduced-path (drop 1 (rseq (subvec path 1)))
        indexed-path (partition 2 (interleave reduced-path (cycle (range 10))))]
    (reduce #(update-maze %1 (first %2) (str (second %2))) @mp/maze* indexed-path )))

(defn start-search
  "start a new search; print path overlaid result if doprint is true"
  ([]
   (start-search :bfs true))
  ([type-of-search doprint]
   (reset! mp/visited* #{})
   ;; TODO fix this to initialize for other types of searches
   (let [init-fn (if (= type-of-search :bfs) bfs-start dfs-start)
         start-node (init-fn)]
     (let [{:keys [found path]} (search-maze start-node)]
       (if found
         (do
           (when doprint
             (doseq [ln (overlay-path (drop-last 1 path))]
               (println ln)))
           (println "Found: Path length: " (count path))
           (print-maze-params))
         (println "No path was found"))))))

(defn start-astar-search
  "start a new astar search; print path overlaid result if doprint is true"
  ([]
   (start-astar-search true))
  ([doprint]
   (dosync (ref-set mp/a-visited* (hash-map)))
   (send mp/max-frontier-size (constantly 0) 0)
   (let [start-node (astar-start)]
     (let [{:keys [found]} (astar-search-maze start-node @mp/goal*)]
       (if found
         (do
           (let [path (extract-path)]
             (when doprint
               (doseq [ln (a-overlay-path path)]
                 (println ln)))
             (println "Found: Path length: " (count path))
             (print-maze-params)))
         (println "No path was found"))))))

(defn new-maze-problem
  [size sparsity]
  (make-full-maze size sparsity)
  (start-search)
  "Done with problem"
  #_(start-bfs-search)
  #_(start-dfs-search))

;; TODO Use Nippy
(defn save-maze
  [fname]
  (let [f (clojure.java.io/file (str fname ".maz"))]
    (nippy/freeze-to-file f {:start @mp/start*
                             :goal @mp/goal*
                             :size @mp/size*
                             :maze @mp/maze*}))
  (println "Maze saved as" fname))

(defn read-maze
  [fname]
  (let [f (clojure.java.io/file (str fname ".maz"))
        parms (nippy/thaw-from-file f)]
    (reset! mp/start* (:start parms))
    (reset! mp/goal* (:goal parms))
    (reset! mp/size* (:size parms))
    (reset! mp/maze* (:maze parms)))
  (println "Read maze" fname))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (make-full-maze 50 30)
  (start-search) ; bfs
  (start-search :dfs true)) ; dfs
