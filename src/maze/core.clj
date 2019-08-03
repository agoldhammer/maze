(ns maze.core
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
(def start* (atom []))
(def goal* (atom []))
(def maze* (atom []))
(def size* (atom 0))
(def sparsity* (atom 1))
(def visited* (atom #{}))
(def a-visited* (ref (hash-map)))

(defn init-frontier
  "return a frontier with 1 node: loc start and path at start"
  []
  (let [start-loc @start*]
    [{:loc start-loc :path [start-loc]}]))

;; TODO
; this is a dummy for testing, remove later!!
(def dummy-vec-of-nodes
  [{:loc [1 2] :path [[0 1]]}
   {:loc [2 3] :path [[0 1] [1 2]]}])

(def dummy-vec-with-heuristics
  [[[1 2] [[0 1] 1 1] 1 5]
   [[2 3] [[0 1] [1 2]] 1 2]
   [[3 4] [[0 1] [1 2]] 1 1]
   [[4 5] [[0 1] [1 2]] 1 27]
   [[5 6] [[0 1] [1 2]] 1 1]
   [[6 7] [[0 1] [1 2]] 1 3]])

(defn priority-queue
  ([]
   (PriorityBlockingQueue.))
  ([^java.util.Collection xs]
   (PriorityBlockingQueue. xs))
  ([init-size ordering]
   (PriorityBlockingQueue. init-size (comparator ordering))))

;; TODO is it worth making a seprate type for a node?

(deftype Node [loc path g h])

(defn node-total-cost [^Node node]
  (+ (.g node) (.h node)))

(defn node-comp [^Node n1 ^Node n2]
  (< (node-total-cost n1) (node-total-cost n2)))

(defn calc-heuristic
  "given two locs, loca and locb, calculate the Manhattan distance"
  [loca locb]
  (let [[xdist ydist] (mapv - loca locb)]
    (+ (Math/abs xdist) (Math/abs ydist))))

(def node-vector (mapv #(apply ->Node %) dummy-vec-with-heuristics))

;; TODO For testing only remove later ****
(def pq (priority-queue 100 node-comp))



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
  (add-nodes [this vec-of-nodes] "add nodes in vector and return new frontier")
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
  (add-nodes [this v-of-nodes]
    (->Fifo (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))

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
  (add-nodes [this v-of-nodes]
    (->StackD (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))


(deftype PriQ [pq])

(extend-protocol Frontier
  PriQ
  (countf [this]
    (.size (.pq this)))
  (add-nodes [this v-of-nodes]
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

(defn astar-start
  "return initial frontier (PriQ) for astar"
  []
  (let [{:keys [loc path]} ((init-frontier) 0)
        _ (println loc path)]
    (let [queue (priority-queue 1000 node-comp)
          pq (->PriQ queue)]
      (add-nodes pq [(->Node loc path 0 (calc-heuristic loc @goal*))])
      pq)))

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
     (reset! start* start)
     (reset! goal*  goal)
     (reset! size* size)
     (reset! sparsity* sparsity)
     (reset! maze* 
             (->
              base
              (update-maze start "S")
              (update-maze goal "G"))))
   (when doprint
     (print-maze))))

(defn print-maze-params
  "print just maze parameters, not maze itself"
  []
  (println "Size: " @size* " Sparsity: " @sparsity*)
  (println "Start:" @start*)
  (println "Goal:" @goal*))

(defn print-maze
  "if doall, print maze with params; else just print params"
  ([] (print-maze true))
  ([doall]
   (when doall
     (doseq [ln @maze*]
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
     (get-in @maze* loc)))

(defn successors
  "return set of successors to loc"
  [loc]
  (let [[x y] loc
        size @size*
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
  (contains? @visited* loc))

(defn- at-goal?
  [loc]
  (= loc @goal*))

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
          (do (swap! visited* conj loc)
              (cond
                (nil? loc) {:found false}
                (at-goal? loc) {:found true :path path}
                :else (let [succ (successors loc)
                            unvisited (filter #(not (visited? %)) succ)
                            nodes (mapv #(loc->node % path) unvisited)
                            new-frontier (add-nodes frontier nodes)]
                        (recur new-frontier))))
          (recur (remainder frontier))))
      {:found false})))


(defn filter-function
  "pass a node if loc is unvisited or if its total-cost is less than old cost"
  [node]
  (let [loc (.loc node)
        new-f (node-total-cost node)
        old-f (get @a-visited* loc)]
    (or (nil? old-f)
        (< new-f old-f))))

(defn loc->Node
  "from loc, path, goal make N Node"
  [loc path goal g-of-new-node]
  (let [heuristic (calc-heuristic loc goal)
        new-node (loc->node loc path)]
    (->Node (:loc new-node) (:path new-node) g-of-new-node heuristic)))

(defn unvisited-cheaper-successors
  "find unvisited cheaper successors;
   returns as seq of Nodes"
  [loc path goal g-of-new-node]
  (let [succ (successors loc)
        nodes (map #(loc->Node %1 path goal g-of-new-node) succ)]
    (filter filter-function nodes)))

(defn astar-search-maze
  "start-frontier is a priority queue of Node types"
  [^:dynamic start-frontier goal]
  (loop [frontier start-frontier]
    #_(println "Frontier length: " (countf frontier))
    (if (deserted? frontier)
      {:found false}
      (let [working-node (get-next frontier)
            loc (.loc working-node)
            path (.path working-node)]
        (if (at-goal? loc)
          {:found true :path path}
          ; else
        
          (let [current-f (node-total-cost working-node)
                old-f (get @a-visited* loc nil)
               ;; _ (println "Working node"  (.loc working-node) (.g working-node)
                ;;           (.h working-node))
                ;; _ (println "current-cost, old-cost" current-cost old-cost)
                should-expand? (or (nil? old-f) ; working node has not yet been visited
                                   (< current-f old-f))] ; cheaper route found
            ; so add it to a-visited with current cost and recur
            (when should-expand?
              (do 
                (dosync (alter  a-visited* assoc loc current-f))
                #_(println "in should expand" @a-visited* loc current-f)
                (let [new-g (inc (.g working-node))
                      unvisited (unvisited-cheaper-successors loc path goal new-g)]
                  #_(println "unvisited" (count unvisited))
                  (add-nodes frontier unvisited))))
            #_(println "frontier" frontier)
            (recur frontier)))))))

(defn overlay-path
  "print maze with search path overlay"
  [path]
  ;; drop the starting node
  (let [reduced-path (drop 1 path)
        maze @maze*
        index (map #(mod % 10) (range (count reduced-path)))
        indexed-path (partition 2 (interleave index reduced-path))]
    (reduce #(update-maze %1 (second %2) (str (first %2))) maze indexed-path)))

(defn start-search
  "start a new search; print path overlaid result if doprint is true"
  ([]
   (start-search :bfs true))
  ([type-of-search doprint]
   (reset! visited* #{})
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
   (dosync (ref-set a-visited* (hash-map)))
   ;; TODO fix this TO INIT properly
   (let [start-node (astar-start)]
     (let [{:keys [found path]} (astar-search-maze start-node @goal*)]
       (if found
         (do
           (when doprint
             (doseq [ln (overlay-path (drop-last 1 path))]
               (println ln)))
           (println "Found: Path length: " (count path))
           (print-maze-params))
         (println "No path was found"))))))

(defn new-maze-problem
  [size sparsity]
  (make-full-maze size sparsity)
  (start-search)
  "Done with problem"
  #_(start-bfs-search)
  #_(start-dfs-search))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (make-full-maze 50 30)
  (start-search) ; bfs
  (start-search :dfs true)) ; dfs
