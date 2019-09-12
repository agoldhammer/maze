(ns maze.utils
  (:require [maze.params :as mp]))

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

(defn make-bare-maze
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

(declare print-maze)
(defn make-maze
  "make maze of given size and sparsity with S and G
   indicating start and goal positions; print if doprint is true"
  ([size sparsity]
   (make-maze size sparsity true))
  ([size sparsity doprint]
   (let [base (make-bare-maze size sparsity)
         [start goal] (choose-start-finish size)]
     (alter-var-root #'mp/start* (constantly start))
     (alter-var-root #'mp/goal*  (constantly goal))
     (alter-var-root #'mp/size* (constantly size))
     (alter-var-root #'mp/sparsity* (constantly sparsity))
     (alter-var-root #'mp/maze*
                     (constantly (->
                                   base
                                   (update-maze start "S")
                                   (update-maze goal "G")))))
   (when doprint
     (print-maze))))

(defn print-maze-params
  "print just maze parameters, not maze itself"
  []
  (println "Size:" mp/size* " Sparsity:" mp/sparsity*)
  (println "Start:" mp/start*)
  (println "Goal:" mp/goal*)
  (println "Max frontier size:" @mp/max-frontier-size))

(defn print-maze
  "if doall, print maze with params; else just print params"
  ([] (print-maze true))
  ([doall]
   (when doall
     (doseq [ln mp/maze*]
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
        (get-in mp/maze* loc)))

(defn successors
  "return vector of successors to loc"
  [loc]
  (let [[x y] loc
        size mp/size*
        x- (dec x)
        x+ (inc x)
        y- (dec y)
        y+ (inc y)
        s1 (for [xn (filter #(in-bounds? % size) (list x- x+))] [xn y])
        s2 (for [yn (filter #(in-bounds? % size) (list y- y+))] [x yn])]
    (apply vector (filter not-blocked? (into s1 s2)))))

(defn visited?
  "has loc been visited?"
  [loc]
  (contains? @mp/visited* loc))

(defn at-goal?
  [loc]
  (= loc mp/goal*))

(def log-agent (agent nil))

(defn log [thread-num & mesg]
  (send log-agent #(println (clojure.string/join " " (concat (str thread-num) mesg)) %)))

(defn ticks
  "time function call f in milliseconds"
  [f & args]
  (let [start (System/nanoTime)
        res (apply f args)
        end (System/nanoTime)
        t (/ (- end start) 1e6)]
    {:result res :time t}))

(defn speedup
  "return ration of execution time of 2 functions"
  [f g & args]
  (let [t1 (:time (apply ticks f args))
        t2 (:time (apply ticks g args))]
    (/ t2 t1)))