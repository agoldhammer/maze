(ns maze.core
  (:gen-class))

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
  [sparsity]
  (fn [elt]
    (if (< (rand-int 10) sparsity)
      "x"
      ".")))

(defn put-wall-in-row
  [len sparsity]
  (let [f (change-randomly sparsity)]
    (mapv f (range len))))

(defn make-maze
  [size sparsity]
    (vec (repeatedly size #(put-wall-in-row size sparsity))))

; a loc is a vec pair [row col]
(defn random-loc
  [size]
    ((juxt rand-int rand-int) size))

(defn update-maze
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
  [size sparsity]
  (let [base (make-maze size sparsity)
        [start goal] (choose-start-finish size)]
    (->
     base
     (update-maze start "S")
     (update-maze goal "G"))
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
