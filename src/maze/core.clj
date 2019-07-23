(ns maze.core
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
  (deserted? [this] "Is the frontie empty?")
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


;; https://github.com/clojure/data.priority-map/
(deftype PriQ [nodes])

;; -----------------------------------------

(defn bfs-start
  "return initial frontier (Fifo) for breadth first search"
  []
  (->Fifo (init-frontier)))

(defn dfs-start
  "return initial frontier (Stack) for depth first search"
  []
  (->StackD (init-frontier)))

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
           (if doprint
             (doseq [ln (overlay-path (drop-last 1 path))]
               (println ln))
             (println "Path length: " (count path)))
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
