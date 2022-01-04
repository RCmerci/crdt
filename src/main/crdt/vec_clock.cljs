(ns crdt.vec-clock
  (:refer-clojure :exclude [-compare])
  (:require [crdt.core :refer [GCounter] :as core]
            [clojure.set :as set]))


(defprotocol IVClock
  (-inc [this replica-id])
  (-merge [this other])
  (-compare [this other]))

(deftype VClock [gcounter]
  IVClock
  (-inc [_ replica-id]
    (VClock. (core/-inc gcounter replica-id)))
  (-merge [_ other]
    (VClock. (core/-merge gcounter (.-gcounter other))))
  (-compare [_ other]
    (let [m1 (.-m gcounter)
          m2 (.. other -gcounter -m)]
      (reduce (fn [r k]
                (let [v1 (get m1 k 0)
                      v2 (get m2 k 0)]
                  (case [r (compare v1 v2)]
                    [:EQ -1] :LT
                    [:EQ 1]  :GT
                    [:LT 1]  (reduced :CC)
                    [:GT -1] (reduced :CC)
                    r)))
              :EQ (into (set (keys m1)) (keys m2)))))
  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "VClock[" (.-m gcounter) "]"))))

(set! (.-EMPTY VClock) (VClock. (.-EMPTY GCounter)))

(comment
  (def c1 (-> (.-EMPTY VClock)
              (-inc :id1)
              (-inc :id1)))
  (def c2 (-> (.-EMPTY VClock)
              (-inc :id2)
              (-inc :id2)))
  (-compare c1 c2)
  (set! c1 (-merge c1 c2))
  (set! c2 c1)
  (set! c1 (-> c1
               (-inc :id1)
               (-inc :id1)))
  (-compare c1 c2)
  (set! c2 (-> c2
               (-inc :id2)))
  (-compare c1 c2)
  )
