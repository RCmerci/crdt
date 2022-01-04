(ns crdt.core)

(defprotocol ICRDT
  (-merge [this other]))

(defprotocol ICounter
  (-value [c])
  (-inc [c replica-id])
  (-dec [c replica-id]))

(deftype GCounter [m]
  ICounter
  (-value [_] (->> m vals (reduce +)))
  (-inc [_ replica-id] (GCounter. (assoc m replica-id (inc (or (m replica-id) 0)))))
  (-dec [_ _] (throw (ex-info "GCounter don't implement -dec" nil)))

  ICRDT
  (-merge [this other] (GCounter. (merge-with max m (.-m other))))

  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "GCounter[" m "]"))))

(set! (.-EMPTY GCounter) (->GCounter {}))

(comment
  (def g (.-EMPTY GCounter))
  (prn (-value g))
  (prn g)
  (set! g (-inc g "id2"))
  (set! g (-inc g "id3"))
  (prn (-value g))
  (prn g)


  (def g1 (.-EMPTY GCounter))
  (def g2 (.-EMPTY GCounter))
  (set! g1 (-> g1
               (-inc :id1)
               (-inc :id1)))
  (set! g2 (-> g2
               (-inc :id1)
               (-inc :id2)))
  (def g3 (-merge g1 g2))
  (prn g1)
  (prn g2)
  (prn g3)

  )


(deftype PNCounter [inc-m dec-m]
  ICounter
  (-value [_] (- (->> inc-m vals (reduce +))
                 (->> dec-m vals (reduce +))))
  (-inc [_ replica-id]
    (PNCounter. (assoc inc-m replica-id (inc (or (inc-m replica-id) 0))) dec-m))
  (-dec [_ replica-id]
    (PNCounter. inc-m (assoc dec-m replica-id (inc (or (dec-m replica-id) 0)))))

  ICRDT
  (-merge [this other]
    (PNCounter. (merge-with max inc-m (.-inc-m other))
                (merge-with max dec-m (.-dec-m other))))

  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "GCounter[" inc-m "," dec-m "]"))))

(set! (.-EMPTY PNCounter) (->PNCounter {} {}))

(comment
  (def c (.-EMPTY PNCounter))
  (prn (-value c))
  (set! c (-inc c :id1))
  (set! c (-inc c :id2))
  (prn (-value c))
  (prn c)
  (set! c (-dec c :id2))
  (prn (-value c))


  (def g1 (.-EMPTY PNCounter))
  (def g2 (.-EMPTY PNCounter))
  (set! g1 (-> g1
               (-inc :id1)
               (-inc :id2)
               (-dec :id1)))
  (set! g2 (-> g2
               (-inc :id1)
               (-inc :id1)
               (-dec :id2)
               (-dec :id2)))
  (def g3 (-merge g1 g2))
  (prn g1 g2 g3)
  (prn (-value g3))
  )
