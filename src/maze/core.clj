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
  [row sparsity]
  (let [f (change-randomly sparsity)]
    (vec 
     (for [n (range (count row))]
       (f (nth row n))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
