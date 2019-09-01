(ns maze.buffers
  #_(:require [maze.params :as mp])
  (:import [java.util.concurrent ConcurrentLinkedQueue]))

(defprotocol Buffer
  "protocol for counting buffer"
  (size [this] "return count of msgs in buffer")
  (take-buff [this] "take next basic msg")
  (put-buff [this payload] "add basic msg")
  (vide? [this] "buffer empty?")
  (quickpeek [this] "Peek at next"))

(deftype CountedBuffer [buff counter clock tmax])

(defn new-counted-buffer
  "returns new Counted-Buffer"
  []
  (->CountedBuffer (ConcurrentLinkedQueue.) (atom 0) (atom 0) (atom 0)))

(extend-protocol Buffer
  CountedBuffer
  (size [this]
    (.size (.buff this)))
  (take-buff [this]
    (let [[clock payload] (.poll (.buff this))]
      (swap! (.counter this) dec)
      (swap! (.tmax this) max clock)
      [clock payload]))
  (put-buff [this payload]
    (swap! (.counter this) inc)
    (.add (.buff this) [@(.clock this) payload]))
  (vide? [this]
    (.isEmpty (.buff this)))
  (quickpeek [this]
    (.peek (.buff this))))





