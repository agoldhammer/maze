(ns maze.overlay
  (:require [maze.params :as mp]
            [maze.utils :as mu]))

(defn overlay-path
  "print maze with search path overlay"
  [path]
  ;; drop the starting node
  (let [reduced-path (drop 1 path)
        maze mp/maze*
        index (map #(mod % 10) (range (count reduced-path)))
        indexed-path (partition 2 (interleave index reduced-path))]
    (reduce #(mu/update-maze %1 (second %2) (str (first %2))) maze indexed-path)))

(defn extract-path
  "extract path from a-visited; includes start and goal points
   extracted path is in reverse order, from goal to start"
  []
  ;; drop the starting node
  (let [a-visited @mp/a-visited*]
    (loop [path [mp/goal*]]
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
    (reduce #(mu/update-maze %1 (first %2) (str (second %2))) mp/maze* indexed-path)))