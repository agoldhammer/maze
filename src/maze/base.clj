(ns maze.base
  (:require [maze.params :as mp])
  (:import [java.util.concurrent PriorityBlockingQueue])
  )

;; Node type has field loc (location), parent, g (cost to node), and
;; h (heuristic); total cost f = g + h
(deftype Node [loc parent g h])

(defn node->tuple
  "returns [loc parent g h] from Node"
  [node]
  [(.loc node) (.parent node) (.g node) (.h node)])

(defn node-total-cost [^Node node]
  (+ (.g node) (.h node)))

(defn node-comp [^Node n1 ^Node n2]
  (< (node-total-cost n1) (node-total-cost n2)))

(defn calc-heuristic
  "given two locs, loca and locb, calculate the Manhattan distance"
  [loca locb]
  (let [[xdist ydist] (mapv - loca locb)]
    (+ (Math/abs xdist) (Math/abs ydist))))

(defn print-node
  [node]
  (let [[loc parent g h] (node->tuple node)]
    (println "Node: loc->" loc "parent->" parent "g->" g "h->" h)))

;;======================

;; The Frontier protocol expects this to be a type containing a sequence of nodes
;; for use by the search algorithm
;; Three concrete types are implemented: a Fifo, a Stack, and a Priority queue
;; A node is a map containing a :loc, a :path to that :loc, and optionally
;; a :heuristic for use by the A* algo

(defprotocol Frontier
  "protocol for handling various frontier types"
  (countf [this] "return count of node list")
  (get-next [this] "get next non mutating")
  (get-next! [this] "get next node mutating")
  (raw-remainder [this] "return the underlying remainder node sequence without converting to type")
  (remainder [this] "remainder (as type) after dropping the next node")
  (add-nodes! [this vec-of-nodes] "add nodes in vector and return new frontier")
  (deserted? [this] "Is the frontier empty?")
  (quickpeek [this] "Peek at top priority element"))

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
  (add-nodes! [this v-of-nodes]
    (->Fifo (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))

(declare new-priq)

#_(defn split-frontier
  "divide frontier into n or n+1 sub-frontiers"
  [frontier n]
  {:pre [(> n 0)]}
  ;; randomize assignment to sub-frontiers by shuffling
  (let [size (quot (countf frontier) n)
        vec-of-nodes (shuffle (into [] (.toArray (.pq frontier))))]
    (doall
     (map #(new-priq %1 1000) (partition-all size vec-of-nodes)))))

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
  (add-nodes! [this v-of-nodes]
    (->StackD (into (raw-remainder this) v-of-nodes)))
  (deserted? [this]
    (empty? (.nodes this))))

(deftype PriQ [pq])

(extend-protocol Frontier
  PriQ
  (countf [this]
    (.size (.pq this)))
  (add-nodes! [this v-of-nodes]
    (doseq [v v-of-nodes]
      (.add (.pq this) v)))
  ;; note!! remainder is not used, get next strips head of queue
  (get-next! [this]
    (if (deserted? this)
      nil
      (.take (.pq this))))
  (remainder [this]
    (.take (.pq this))
    this)
  (deserted? [this]
    (nil? (.peek (.pq this))))
  (quickpeek [this]
    (.peek (.pq this))))


(defn init-frontier
  "return a frontier with 1 node: loc start and path at start"
  []
  (let [start-loc @mp/start*]
    [{:loc start-loc :path [start-loc]}]))

(defn priority-queue
  ([]
   (PriorityBlockingQueue.))
  ([^java.util.Collection xs]
   (PriorityBlockingQueue. xs))
  ([init-size ordering]
   (PriorityBlockingQueue. init-size (comparator ordering))))

(defn new-priq
  "make a new priority queue from a vector of Nodes"
  [vec-of-Nodes size]
  (let [queue (priority-queue size node-comp)
        pq (->PriQ queue)]
    (add-nodes! pq vec-of-Nodes)
    pq))


(defn start-node
  "return start Node"
  []
  (let [start @mp/start*]
    (->Node start nil 0 (calc-heuristic start @mp/goal*))))

(defn astar-start-frontier
  "return initial frontier (PriQ) for astar"
  []
  (let [start @mp/start*]
    (new-priq [(->Node start nil 0 (calc-heuristic start @mp/goal*))] 1000)))

(defn loc->Node
  "from loc, path, goal make N Node"
  [loc parent goal g-of-new-node]
  (let [heuristic (calc-heuristic loc goal)]
    (->Node loc parent g-of-new-node heuristic)))

