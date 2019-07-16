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

;; create an n x n maze
(defn create-maze
  [n]
  {:size n
   :maze (vec (repeat 8 (vec (range n))))
   })

(defn check-coord
  [loc limit]
  (if 
   (or (< loc 0)
       (> loc limit))
    :err
    loc))

(def maze8 (create-maze 8))

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
   indicating start and goal positions"
  [size sparsity]
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
  (print-maze))

(defn print-maze
  []
  (doseq [ln @maze*]
    (println ln))
  (println "Size: " @size* " Sparsity: " @sparsity*)
  (println "Start:" @start*)
  (println "Goal:" @goal*))

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

(defn search-at
  "search at loc, having come from"
  [loc came-from]
  (on-debug (println "searching " loc))
  (swap! visited* conj loc)
  (if (at-goal? loc)
    {:found true
     :path came-from}
    (let [coming-from (conj came-from loc)
          frontier (filter #(not (visited? %)) (successors loc))]
      (on-debug (println "coming from" coming-from))
      (on-debug (println "frontier" frontier)
                (println "visited" @visited*))
      (loop [next (first frontier)
             remaining (rest frontier)]
        (if next
          (let [path (search-at next coming-from)]
            (if (:found path)
              path
              (recur (first remaining) (rest remaining))))
          {:found false})))))

; a node is a map with components loc and parent
(defn loc->node
  "create a node from a loc by adding parent"
  [loc path]
  {:loc loc :path (conj path loc)})

(defn start-bfs
  "breadth-first search"
  []
  (reset! visited* #{})
  (let [start @start*]
    (loop [frontier [{:loc start :path [start]}]]
      (if (not (empty? frontier))
        (let [working-node (first frontier)
              loc (:loc working-node)
              path (:path working-node)]
          (if (not (visited? loc))
            
            (do (swap! visited* conj loc)
                (cond
                  (nil? loc) {:found false}
                  (at-goal? loc) {:found true :path path}
                  :else (let [succ (successors loc)
                              unvisited (filter #(not (visited? %)) succ)
                              nodes (mapv #(loc->node % path) unvisited)
                              new-frontier (vec (into (subvec frontier 1) nodes))]
                          #_(println "new-frontier " new-frontier)
                          #_(println "visited" @visited*)
                          #_(println (vector new-frontier new-path @visited*))
                          (recur new-frontier))))
            (recur (subvec frontier 1))))
        {:found false}))
    ))

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
  "start a new search"
  []
  (reset! visited* #{})
  (let [{:keys [found path]} (search-at @start* [])]
    (if found
      (doseq [ln (overlay-path path)]
        (println ln))
      (println "No path was found"))))

(defn start-bfs-search
  "start a new bfs search"
  []
  (reset! visited* #{})
  (let [{:keys [found path]} (start-bfs)]
    (if found
      (doseq [ln (overlay-path (drop-last 1 path))]
        (println ln))
      (println "No path was found"))))

(defn new-maze-problem
  [size sparsity]
  (make-full-maze size sparsity)
  (start-search))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
