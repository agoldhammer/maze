(ns maze.core
  (:gen-class))

(def DEBUG false)

(defmacro on-debug [& body]
  `(when DEBUG
     (do ~@body)))

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
             (update-maze goal "G")))))

(defn print-maze
  []
  (pprint @maze*)
  (println "Size: " @size* " Sparsity: " @sparsity*)
  (println "Start:" @start*)
  (println "Goal:" @goal*))

(defn in-bounds?
  "check coord in bounds for size"
  [w size]
  (if (and (>= w 0) (< w size))
    w
    false))

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
    (into s1 s2))
  )

(defn visited?
  "has loc been visited?"
  [loc]
  (contains? @visited* loc))

(defn search-at
  "search at loc, having come from"
  [loc came-from]
  (on-debug (println "searching " loc))
  (swap! visited* conj loc)
  (if (= loc @goal*)
    :found
    (let [coming-from (conj came-from loc)
          frontier (filter #(not (visited? %)) (successors loc))]
      (on-debug (println "coming from" coming-from))
      (on-debug (println "frontier" frontier)
                (println "visited" @visited*))
      (loop [next (first frontier)
             remaining (rest frontier)]
        (if (= :found (search-at next coming-from))
          :found
          (recur (first remaining) (rest remaining))))
      )))

(defn start-search
  "start a new search"
  []
  (reset! visited* #{})
  (search-at @start* []))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
