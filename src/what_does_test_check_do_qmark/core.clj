(ns what-does-test-check-do-qmark.core)

(defn ranks []
  [1 2 3 4 5 6 7 8 9 10 \J \Q \K \A])

(defn rank? [rank]
  ((apply hash-set (ranks)) rank))

(defn rank->ordinal [rank]
  {:pre (rank? rank)}
  (.indexOf (ranks) rank))

(defn ordinal->rank [ordinal]
  {:pre [(integer? ordinal)
         (<= 0 ordinal)
         (>= 14 ordinal)]}
  (get (ranks) ordinal))

(defn card-sort
  "Returns cards sorted in high-to-low order."
  [cards]
  (reverse (sort-by (comp rank->ordinal first) cards)))

(defn consecutive? [ordinal-ranks]
  {:pre [(every? integer? ordinal-ranks)]}
  ordinal-ranks
  (let [[r1 r2] ordinal-ranks]
    r1
    r2
    (let [consecutive-numbers (iterate (if (< r1 r2) inc dec) (first ordinal-ranks))]
      (every? true? (map = ordinal-ranks consecutive-numbers)))))

(defn straight-flush? [hand-cards]
  hand-cards
  (let [high-to-low-cards (card-sort hand-cards)
        ordinal-ranks (map (comp rank->ordinal first) high-to-low-cards)
        suits (map second high-to-low-cards)]
    (if (and (consecutive? ordinal-ranks)
             (apply = suits))
      straight-flush?)))

(defn partition-cards [hand-cards]
  (let [high-to-low-cards (card-sort hand-cards)
        partitioned-cards (partition-by first high-to-low-cards)]
    partitioned-cards))

(defn four-of-a-kind? [hand-cards]
  (let [high-to-low-cards (card-sort hand-cards)
        partitioned-cards (partition-by first high-to-low-cards)]
    (if (some (partial = 4) (map count partitioned-cards))
      four-of-a-kind?)))

(defn partition-counts [hand-cards]
  (let [high-to-low-cards (card-sort hand-cards)
        partitioned-cards (partition-by first high-to-low-cards)
        counts (sort (map count partitioned-cards))]
    counts))

(defn full-house? [hand-cards]
  (let [counts (partition-counts hand-cards)]
    (if (= [2 3] counts)
      full-house?)))

(defn flush? [hand-cards]
  (let [suits (map second hand-cards)]
    (if (apply = suits)
      flush?)))

(defn pair? [hand-cards]
  (let [counts (partition-counts hand-cards)]
    (if (some (partial = 2) counts)
      pair?)))

(defn two-pairs? [hand-cards]
  (let [counts (partition-counts hand-cards)]
    (if (= [1 2 2] counts)
      two-pairs?)))

(defn three-of-a-kind? [hand-cards]
  (let [counts (partition-counts hand-cards)]
    (if (some (partial = 3) counts)
      three-of-a-kind?)))

(defn straight? [hand-cards]
  (let [high-to-low-cards (card-sort hand-cards)
        ranks (map first high-to-low-cards)]
    (if (consecutive? (map rank->ordinal ranks))
      straight?)))

(defn high-card? [hand-cards]
  high-card?)

(defn hands []
  [straight-flush? four-of-a-kind? full-house? flush? straight? three-of-a-kind? two-pairs? pair? high-card?])

(defn apply-hand-predicate [hand-cards hand-predicate]
  (hand-predicate hand-cards))

(defn hand [hand-cards]
  (some (partial apply-hand-predicate hand-cards) (hands)))

(defn poker-hand
  "Returns {:hand      hand
            :color     color}"
  [{:keys [color cards] :as player-hand}]
  {:hand  (hand cards)
   :cards cards
   :color color})

(defn suits []
  #{\H \C \S \D})

(defn suit? [maybe-suit]
  ((suits) maybe-suit))

(defn card? [maybe-card]
  (and (= 2 (count maybe-card))
       (rank? (first maybe-card))
       (suit? (second maybe-card))))

(defn hand? [hand]
  (and (= 5 (count hand))
       (every? card? hand)))

(defn high-quadruplet [cards]
  (let [partitioned-cards (partition-cards cards)]
    (some (fn [card-partition]
            (if (= 4 (count card-partition))
              (rank->ordinal (ffirst card-partition))))
          partitioned-cards)))

(defn high-triplet [cards]
  (let [partitioned-cards (partition-cards cards)]
    (some (fn [card-partition]
            (if (= 3 (count card-partition))
              (rank->ordinal (ffirst card-partition))))
          partitioned-cards)))

(defn high-pair [cards]
  (let [partitioned-cards (partition-cards cards)]
    (some (fn [card-partition]
            (if (= 2 (count card-partition))
              (rank->ordinal (ffirst card-partition))))
          partitioned-cards)))

(defn low-pair [cards]
  (let [partitioned-cards (partition-cards cards)
        pairs (filter (comp (partial = 2) count) partitioned-cards)]
    (rank->ordinal (ffirst (first (drop 1 pairs))))))

(defn high-card [hand-cards]
  (rank->ordinal (ffirst (card-sort hand-cards))))

(defn component->string [component-fn]
  (get {high-card "high card"
        high-pair "high pair"
        low-pair "low pair"
        high-triplet "high triplet"
        high-quadruplet "high quadruplet"}
       component-fn))

(defn hand->ranking-fns []
  [[high-card?       [high-card]]
   [pair?            [high-pair high-card]]
   [two-pairs?       [high-pair low-pair high-card]]
   [three-of-a-kind? [high-triplet high-card]]
   [straight?        [high-card]]
   [flush?           [high-card]]
   [full-house?      [high-triplet high-pair]]
   [four-of-a-kind?  [high-quadruplet high-card]]
   [straight-flush?  [high-card]]])

(defn hand-rank [hand]
  (.indexOf (map first (hand->ranking-fns)) hand))

(defn hand->string []
  {straight-flush?  "straight flush"
   four-of-a-kind?  "four of a kind"
   full-house?      "full house"
   flush?           "flush"
   straight?        "straight"
   three-of-a-kind? "three of a kind"
   two-pairs?       "two pairs"
   pair?            "pair"
   high-card?       "high card"})

(defn ranking-vec [player]
  (let [hand  (:hand player)
        cards (:cards player)
        hand-rank (hand-rank hand)
        hand-ranking-fns (second (get (hand->ranking-fns) hand-rank))
        other-ranks (map (fn [ranking-fn rank]
                           [ranking-fn rank])
                         hand-ranking-fns
                         ((apply juxt hand-ranking-fns) cards))]
    (into [[hand hand-rank]] other-ranks)))

(defn winning-component [win-r-vec lose-r-vec]
  {:pre [(not (= win-r-vec lose-r-vec))]}
  (loop [[rank-n & next-rank-ns] (range (count win-r-vec))]
    (if (not (nil? rank-n))
      (let [current-lose-rank (get lose-r-vec rank-n)
            current-win-rank  (get win-r-vec rank-n)]
        (if (< (second current-lose-rank) (second current-win-rank))
          current-win-rank
          (recur next-rank-ns))))))

(defn ordinal-rank->string [ordinal-rank]
  (let [named-rank (get {10 "Jack"
                         11 "Queen"
                         12 "King"
                         13 "Ace"}
                        ordinal-rank)]
    (if named-rank
      named-rank
      ordinal-rank)))

(defn winning-hand [player-1 player-2]
  {:pre [(hand? (:cards player-1))
         (hand? (:cards player-2))]}
  (let [[p1-rank-1 p1-rank-2 p1-rank-3 p1-rank-4] (ranking-vec player-1)
        [p2-rank-1 p2-rank-2 p2-rank-3 p2-rank-4] (ranking-vec player-2)
        p1-ranking-vec                            [p1-rank-1 p1-rank-2 p1-rank-3 p1-rank-4]
        p2-ranking-vec                            [p2-rank-1 p2-rank-2 p2-rank-3 p2-rank-4]
        [winning-player win-r-vec lose-r-vec]     (case (compare (mapv second p1-ranking-vec)
                                                                 (mapv second p2-ranking-vec))
                                                    -1 [player-2 p2-ranking-vec p1-ranking-vec]
                                                    1 [player-1 p1-ranking-vec p2-ranking-vec]
                                                    0 [nil])]
    (if winning-player
      (let [[winning-component winning-component-rank] (winning-component win-r-vec lose-r-vec)]
        [winning-player winning-component winning-component-rank])
      [nil])))

(defn winner-string [{:keys [color hand] :as player} component ordinal-rank]
  (let [basic-win-str (format "%s wins. with %s"
                              color
                              (get (hand->string) hand))]
    (if (component->string component)
      (format "%s: %s %s"
              basic-win-str
              (component->string component)
              (ordinal-rank->string ordinal-rank))
      basic-win-str)))

(defn winner
  "Takes 2 poker players and outputs the winner and the hand they had."
  [{cards-1 :cards color-1 :color :as player-1} {cards-2 :cards color-2 :color :as player-2}]
  (let [hand-1                                          (poker-hand player-1)
        hand-2                                          (poker-hand player-2)
        [w-player w-component w-component-ordinal-rank] (winning-hand hand-1 hand-2)]
    (if w-player
      (winner-string w-player w-component w-component-ordinal-rank)
      "Tie.")))
