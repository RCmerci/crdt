(ns crdt.core
  (:refer-clojure :exclude [-compare]))

(set! *warn-on-infer* false)

(defn start [] (prn "start"))

(defprotocol ICRDT
  (-value [c])
  (-merge [this other]))

(defprotocol ICounter
  (-inc [c replica-id])
  (-dec [c replica-id]))

(deftype GCounter [m]
  ICounter
  (-inc [_ replica-id] (GCounter. (assoc m replica-id (inc (or (m replica-id) 0)))))
  (-dec [_ _] (throw (ex-info "GCounter didn't implement -dec" nil)))

  ICRDT
  (-value [_] (->> m vals (apply +)))
  (-merge [this ^GCounter other] (GCounter. (merge-with max m (.-m other))))

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

(defprotocol IVClock
  (-compare [this other]))


(deftype VClock [^GCounter gcounter]
  Object
  (inc [this replica-id]
    (VClock. (-inc gcounter replica-id)))
  ICRDT
  (-merge [_ other]
    (VClock. (-merge gcounter (.-gcounter other))))
  (-value [_] (-value gcounter))
  IVClock
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
              (.inc :id1)
              (.inc :id1)))
  (def c2 (-> (.-EMPTY VClock)
              (.inc :id2)
              (.inc :id2)))
  (prn (-compare c1 c2))
  (set! c1 (-merge c1 c2))
  (set! c2 c1)
  (set! c1 (-> c1
               (.inc :id1)
               (.inc :id1)))
  (prn (-compare c1 c2))
  (set! c2 (-> c2
               (.inc :id2)))
  (prn (-compare c1 c2))
  )

(deftype PNCounter [inc-m dec-m]
  ICounter
  (-inc [_ replica-id]
    (PNCounter. (assoc inc-m replica-id (inc (or (inc-m replica-id) 0))) dec-m))
  (-dec [_ replica-id]
    (PNCounter. inc-m (assoc dec-m replica-id (inc (or (dec-m replica-id) 0)))))

  ICRDT
  (-value [_] (- (->> inc-m vals (reduce +))
                 (->> dec-m vals (reduce +))))
  (-merge [this ^PNCounter other]
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


(defprotocol IRegister
  (-set [this v t]))

(deftype LWWReg [v time]
  IRegister
  (-set [this newv t] (if (< time t) (LWWReg. newv t) this))

  ICRDT
  (-value [this] v)
  (-merge [this other] (if (< time (.-time other)) other this)))

(set! (.-EMPTY LWWReg) (LWWReg. nil nil))


(deftype GSet [set]
  ICollection
  (-conj [_ o] (GSet. (conj set o)))
  ICRDT
  (-value [_] set)
  (-merge [_ other] (GSet. (clojure.set/union set (.-set other)))))

(set! (.-EMPTY GSet) (GSet. #{}))

(deftype PSet [add-set rem-set]
  ICollection
  (-conj [_ o] (PSet. (conj add-set o) rem-set))
  ISet
  (-disjoin [_ v] (PSet. add-set (conj rem-set v)))
  ICRDT
  (-value [_] (clojure.set/difference add-set rem-set))
  (-merge [_ ^PSet other] (PSet. (clojure.set/union add-set (.-add-set other))
                                 (clojure.set/union rem-set (.-rem-set other)))))
(set! (.-EMPTY PSet) (PSet. #{} #{}))

(comment
  (def s1 (.-EMPTY PSet))
  (def s2 (.-EMPTY PSet))
  (set! s1 (-> s1
               (conj 1)
               (conj 2)
               (conj 3)))
  (set! s2 (-> s2
               (conj 1)
               (disj 2)))
  (def s3 (-merge s1 s2))
  (prn (-value s3))
  (set! s3 (conj s3 2))
  (prn (-value s3)))


(defprotocol IReplicaAddRemove
  (-add [this v replica-id])
  (-remove [this v replica-id]))


(deftype ORSet [add-m rem-m]
  ICRDT
  (-value [_]
    (->> rem-m
         (reduce (fn [r [v rem-vc]]
                   (let [add-vc (get r v (.-EMPTY VClock))]
                     (if (= :LT (-compare add-vc rem-vc))
                       (dissoc r v)
                       r)))
                 add-m)
         keys
         set))

  (-merge [_ other]
    (let [add-m* (merge-with (fn [vc1 vc2] (-merge vc1 vc2)) add-m (.-add-m other))
          rem-m* (merge-with (fn [vc1 vc2] (-merge vc1 vc2)) rem-m (.-rem-m other))
          [add-m** rem-m**]
          (reduce (fn [[add-m rem-m] [v rem-vc]]
                    (if-let [add-vc (get add-m v)]
                      (if (= :LT (-compare add-vc rem-vc))
                        [(dissoc add-m v) (assoc rem-m v rem-vc)]
                        [add-m rem-m])
                      [add-m (assoc rem-m v rem-vc)]))
                  [add-m* {}] rem-m*)]
      (ORSet. add-m** rem-m**)))
  IReplicaAddRemove
  (-add [_ v replica-id]
    (let [add-vc (get add-m v)
          rem-vc (get rem-m v)]
      (cond
        (some? add-vc)
        (ORSet. (assoc add-m v (.inc add-vc replica-id)) (dissoc rem-m v))

        (some? rem-vc)
        (ORSet. (assoc add-m v (.inc rem-vc replica-id)) (dissoc rem-m v))

        :else
        (ORSet. (assoc add-m v (.inc (.-EMPTY VClock) replica-id)) rem-m))))

  (-remove [_ v replica-id]
    (let [add-vc (get add-m v)
          rem-vc (get rem-m v)]
      (cond
        (some? add-vc)
        (ORSet. (dissoc add-m v) (assoc rem-m v (.inc add-vc replica-id)))

        (some? rem-vc)
        (ORSet. (dissoc add-m v) (assoc rem-m v (.inc rem-vc replica-id)))

        :else
        (ORSet. add-m (assoc rem-m v (.inc (.-EMPTY VClock) replica-id))))))

  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "ORSet[" add-m "," rem-m "]"))))

(set! (.-EMPTY ORSet) (ORSet. {} {}))

(comment
  (def s1 (-> (.-EMPTY ORSet)
              (-add 1 :id1)
              (-add 2 :id1)
              (-remove 1 :id1)))
  (def s2 (-> (.-EMPTY ORSet)
              (-add 3 :id2)
              (-add 4 :id2)))
  (def s3 (-merge s1 s2))
  (prn s1)
  (prn s2)
  (prn s3)
  (prn (-value s3))
  (def s4 s3)
  (set! s3 (-> s3
               (-remove 2 :id2)
               (-add 2 :id2)
               (-add 3 :id2)))
  (prn (-value s3))
  (set! s4 (-> s4
               (-remove 2 :id1)))
  (prn (-value s4))
  (prn (-value (-merge s3 s4)))
  )


;;;;;;;;;;;;;;;;;
;; delta state ;;
;;;;;;;;;;;;;;;;;

(defprotocol IDeltaCRDT
  (-merge-delta [this delta])
  (-split [this]))

(deftype DeltaGCounter [m ^DeltaGCounter-or-nil delta]
  ICounter
  (-inc [_ replica-id]
    (let [m* (assoc m replica-id (inc (or (m replica-id) 0)))]
      (DeltaGCounter. m* (let [d (or delta (.-EMPTY DeltaGCounter))]
                                (DeltaGCounter. (conj (.-m d) (find m* replica-id)) nil)))))
  (-dec [_ _]
    (throw (ex-info "DeltaGCounter didn't impl -dec" nil)))
  ICRDT
  (-value [_] (->> m vals (apply +)))
  (-merge [_ other] (DeltaGCounter. (merge-with max m (.-m other))
                                    (some-> (merge-with max (some-> delta .-m) (some-> other .-delta .-m))
                                            (DeltaGCounter. nil))))
  IDeltaCRDT
  (-merge-delta [this delta] (-merge this delta))
  (-split [_] [(DeltaGCounter. m nil) delta])

  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "DeltaGCounter[" m "," (pr-str delta) "]"))))

(set! (.-EMPTY DeltaGCounter) (DeltaGCounter. {} nil))

(comment
  (def d1 (-> (.-EMPTY DeltaGCounter)
              (-inc :id1)
              (-inc :id1)))
  (def d2 (-> (.-EMPTY DeltaGCounter)
              (-inc :id2)))
  (set! d1 (-merge d1 d2))
  (set! d2 (-merge d1 d2))
  (prn "d1" d1)
  (prn "d2" d2)
  (set! d1 (first (-split d1)))
  (set! d1 (-> d1
               (-inc :id1)))
  (prn "d1" d1)
  (let [[d1* delta1] (-split d1)]
    (prn "delta1" delta1)
    (prn "d2" d2)
    (set! d2 (-merge-delta d2 delta1))
    (prn "d2" d2)
    (set! d1 d1*))
  (prn "d1" d1)
  (prn "d2" d2)
  )

(deftype DeltaPNCounter [inc-gcounter dec-gcounter]
  ICounter
  (-inc [_ replica-id]
    (DeltaPNCounter. (-inc inc-gcounter replica-id) dec-gcounter))
  (-dec [_ replica-id]
    (DeltaPNCounter. inc-gcounter (-inc dec-gcounter replica-id)))
  ICRDT
  (-value [_] (- (-value inc-gcounter) (-value dec-gcounter)))
  (-merge [_ other] (DeltaPNCounter. (-merge inc-gcounter (.-inc-gcounter other))
                                     (-merge dec-gcounter (.-dec-gcounter other))))
  IDeltaCRDT
  (-merge-delta [this delta] (-merge this delta))
  (-split [_]
    (let [[inc-g inc-delta] (-split inc-gcounter)
          [dec-g dec-delta] (-split dec-gcounter)]
      [(DeltaPNCounter. inc-g dec-g) (DeltaPNCounter. inc-delta dec-delta)]))

  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "DeltaPNCounter[" (pr-str inc-gcounter) "," (pr-str dec-gcounter) "]"))))
(set! (.-EMPTY DeltaPNCounter) (DeltaPNCounter. (.-EMPTY DeltaGCounter) (.-EMPTY DeltaGCounter)))

(comment
  (def d1 (-> (.-EMPTY DeltaPNCounter)
              (-inc :id1)
              (-dec :id1)
              (-inc :id1)))
  (def d2 (-> (.-EMPTY DeltaPNCounter)
              (-inc :id2)
              (-inc :id2)))
  (set! d1 (-merge d1 d2))
  (set! d2 (-merge d1 d2))
  (prn "d1" d1)
  (prn "d2" d2)
  (set! d1 (first (-split d1)))
  (set! d1 (-> d1 (-inc :id1) (-dec :id1)))
  (let [[d1* delta1] (-split d1)]
    (prn "delta1" delta1)
    (set! d2 (-merge-delta d2 delta1))
    (prn "d2" d2)
    (set! d1 d1*))
  (prn "d1" d1)
  (prn "d2" d2)
  (prn "d1 value" (-value d1))
  (prn "d2 value" (-value d2))
  )

(deftype DeltaGSet [set delta]
  ICollection
  (-conj [_ o]
    (let [d (or delta (.-EMPTY DeltaGSet))]
      (DeltaGSet. (conj set o) (DeltaGSet. (conj (.-set d) o) nil))))
  ICRDT
  (-value [_] set)
  (-merge [_ other]
    (DeltaGSet. (clojure.set/union set (.-set other))
                (DeltaGSet. (clojure.set/union (some-> delta .-set)
                                               (some-> other .-delta .-set)) nil)))
  IDeltaCRDT
  (-merge-delta [this delta] (-merge this delta))
  (-split [_] [(DeltaGSet. set nil) delta])
  IPrintWithWriter
  (-pr-writer [o writer opts]
    (write-all writer (str "DeltaGSet[" set "," (pr-str delta) "]"))))

(set! (.-EMPTY DeltaGSet) (DeltaGSet. #{} nil))

(comment
  (def d1 (-> (.-EMPTY DeltaGSet)
              (conj 1)
              (conj 2)))
  (def d2 (-> (.-EMPTY DeltaGSet)
              (conj 3)
              (conj 4)))
  (prn "d1" d1)
  (prn "d2" d2)
  (set! d1 (-merge d1 d2))
  (set! d2 (-merge d1 d2))
  (set! d1 (first (-split d1)))
  (set! d1 (-> d1 (-conj 5) (-conj 6)))
  (let [[d1* delta1] (-split d1)]
    (prn "delta1" delta1)
    (set! d2 (-merge-delta d2 delta1))
    (prn "d2" d2)
    (set! d1 d1*))
  (prn "d1" d1)
  (prn "d2" d2)
  (prn "d1 value" (-value d1))
  (prn "d2 value" (-value d2))
  )
