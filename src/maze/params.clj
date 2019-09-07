(ns maze.params)

(def start* [])
(def goal* [])
(def maze* [])
(def size* 0)
(def sparsity* 1)
(def visited* (atom #{}))
(def a-visited* (ref (hash-map)))
(def max-frontier-size (agent 0))

(def nthreads 4)
(def split-frontier-at 32)