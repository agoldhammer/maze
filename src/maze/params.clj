(ns maze.params)

(def start* (atom []))
(def goal* (atom []))
(def maze* (atom []))
(def size* (atom 0))
(def sparsity* (atom 1))
(def visited* (atom #{}))
(def a-visited* (ref (hash-map)))
(def max-frontier-size (agent 0))

(def nthreads 4)
(def split-frontier-at 32)