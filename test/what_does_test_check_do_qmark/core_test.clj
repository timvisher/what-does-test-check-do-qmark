(ns what-does-test-check-do-qmark.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]

            [what-does-test-check-do-qmark.core :refer :all]))

(def rank-gen (gen/elements [2 3 4 5 6 7 8 9 10 \J \Q \K \A]))

(def suit-gen (gen/elements [\H \C \S \D]))

(def card-gen (gen/tuple rank-gen suit-gen))

(def hand-cards-gen (gen/such-that (fn [hand]
                                     (apply not= hand))
                                   (gen/vector card-gen 5)
                                   20))

(defspec hands-resolve 100
  (prop/for-all [hand-cards hand-cards-gen]
                (hand hand-cards)))

(def color-gen (gen/elements ["White" "Black"]))

(def player-gen (gen/hash-map :color color-gen
                              :cards  hand-cards-gen))

(def white-player-gen (gen/such-that (fn [player]
                                       (= "White" (:color player)))
                                     player-gen
                                     20))

(def black-player-gen (gen/such-that (fn [player]
                                       (= "Black" (:color player)))
                                     player-gen
                                     20))

(def game-gen (gen/such-that (fn [[player-1 player-2]]
                               (let [all-cards (into (:hand player-1) (:hand player-2))]
                                 (= (count all-cards)
                                    (count (apply hash-set all-cards)))))
                             (gen/tuple white-player-gen black-player-gen)
                             20))       ; FIXME this smells. Might
                                        ; consider instead a deck-gen
                                        ; generator that generates a
                                        ; shuffled deck and then hands
                                        ; 5 cards to each player to
                                        ; make it impossible to get 2
                                        ; of the same cards.

(defspec never-error
  (prop/for-all [game game-gen]
                (apply winner game)))

(def deck-gen (gen/shuffle [[2 \C] [3 \C] [4 \C] [5 \C] [6 \C] [7 \C] [8 \C] [9 \C] [10 \C] [\J \C] [\Q \C] [\K \C] [\A \C] [2 \D] [3 \D] [4 \D] [5 \D] [6 \D] [7 \D] [8 \D] [9 \D] [10 \D] [\J \D] [\Q \D] [\K \D] [\A \D] [2 \H] [3 \H] [4 \H] [5 \H] [6 \H] [7 \H] [8 \H] [9 \H] [10 \H] [\J \H] [\Q \H] [\K \H] [\A \H] [2 \S] [3 \S] [4 \S] [5 \S] [6 \S] [7 \S] [8 \S] [9 \S] [10 \S] [\J \S] [\Q \S] [\K \S] [\A \S]]))

(def hand-cards-gen-2 (gen/bind deck-gen
                                (fn [deck]
                                  (gen/return (apply vector (take 5 deck))))))

(def game-gen-2 (gen/bind deck-gen
                          (fn [deck]
                            (let [hand-1 (apply vector (take 5 deck))
                                  hand-2 (apply vector (take 5 (drop 5 deck)))]
                              (gen/return [{:color "White" :cards hand-1}
                                           {:color "Black" :cards hand-2}])))))

(defspec never-error-2
  (prop/for-all [game game-gen-2]
                (apply winner game)))

(defn random-consecutive-ranks []
  (map ordinal->rank (take 5 (range (rand-int (rank->ordinal \J)) (+ 1 (rank->ordinal \A))))))

(defn random-suit []
  (rand-nth (seq (suits))))

(def low-straight-ordinal-rank-gen
  (gen/choose (rank->ordinal 1) (rank->ordinal 10)))

(def straight-ordinal-ranks-gen
  (gen/bind low-straight-ordinal-rank-gen
            (fn [low-straight-ordinal-rank]
              (gen/return (apply vector (range low-straight-ordinal-rank (+ 5 low-straight-ordinal-rank)))))))

(def straight-ranks-gen
  (gen/fmap (fn [straight-ordinal-ranks]
              (mapv ordinal->rank straight-ordinal-ranks))
            straight-ordinal-ranks-gen))

(def straight-flush-gen
  (gen/fmap (fn [straight-ranks]
              (let [suit (random-suit)]
                (shuffle
                 (mapv (fn [rank]
                         [rank suit])
                       straight-ranks))))
            straight-ranks-gen))

(def heterogenous-suits-gen
  (gen/such-that (fn [suits] (apply not= suits))
                 (gen/vector suit-gen 2)))

(defspec test-heterogenous-suits-gen
  (prop/for-all [heterogenous-suits heterogenous-suits-gen]
                (apply not= heterogenous-suits)))

(def straight-suits-gen
  (gen/fmap (fn [heterogenous-suits]
              (into heterogenous-suits (repeatedly 3 random-suit)))
            heterogenous-suits-gen))

(defspec test-straight-suits-gen
  (prop/for-all [straight-suits straight-suits-gen]
                (apply not= straight-suits)))

(def straight-gen
  (gen/fmap (fn [[straight-ranks straight-suits]]
              (shuffle
               (mapv vector
                     straight-ranks
                     straight-suits)))
            (gen/tuple straight-ranks-gen
                       straight-suits-gen)))

(defspec test-straight-hands
  (prop/for-all [straight-cards straight-gen]
                (= straight? (hand straight-cards))))

;;; So far, it seems like it's always better to build up from known
;;; primitives towards the state you're looking for than to generate
;;; random high-level data that then get's filtered down to what
;;; you're looking for.

(defspec test-straight-flush-hands
  (prop/for-all [straight-flush-cards straight-flush-gen]
                (= straight-flush? (hand straight-flush-cards))))

(def quadruplet-ranks-gen
  (gen/bind rank-gen
            (fn [rank]
              (gen/vector (gen/return rank) 4))))

(def four-of-a-kind-ranks-gen
  (gen/such-that (fn [ranks]
                   (apply not= ranks))
                 (gen/fmap (fn [[quadruplet-ranks rank]]
                             (into quadruplet-ranks [rank]))
                           (gen/tuple quadruplet-ranks-gen
                                      rank-gen))))

(def quadruplet-suits-gen
  (gen/shuffle (apply vector (suits))))

(defspec test-quadruplet-suits-gen
  (prop/for-all [quadruplet-suits quadruplet-suits-gen]
                (= 4 (count quadruplet-suits))))

(def four-of-a-kind-suits-gen
  (gen/fmap (fn [[quadruplet-suits suit]]
              (into quadruplet-suits [suit]))
            (gen/tuple quadruplet-suits-gen
                       suit-gen)))

;;; TODO also write using one-of
(def four-of-a-kind-cards-gen
  (gen/such-that (fn [cards]
                   (= 5 (count (apply hash-set cards))))
                 (gen/fmap (fn [[four-of-a-kind-ranks four-of-a-kind-suits]]
                             (shuffle
                              (mapv vector
                                    four-of-a-kind-ranks
                                    four-of-a-kind-suits)))
                           (gen/tuple four-of-a-kind-ranks-gen
                                      four-of-a-kind-suits-gen))))

(defspec test-four-of-a-kind-cards-gen
  (prop/for-all [four-of-a-kind-cards four-of-a-kind-cards-gen]
                (= four-of-a-kind? (hand four-of-a-kind-cards))))

(def straight-flush-vs-four-of-a-kind-gen
  (gen/such-that (fn [[{w-cards :cards} {b-cards :cards}]]
                   (= 10 (count (apply hash-set (into w-cards b-cards)))))
                 (gen/fmap (fn [[straight-flush-cards four-of-a-kind-cards]]
                             [{:color "White" :cards straight-flush-cards}
                              {:color "Black" :cards four-of-a-kind-cards}])
                           (gen/tuple straight-flush-gen
                                      four-of-a-kind-cards-gen))))

;;; TODO This probably can be written in a more generic fashion by
;;; assembling a generator right here using something like gen/tuple,
;;; rather than the made for purpose generator above. See if you can
;;; do that.
(defspec test-straight-flush-vs-four-of-a-kind
  (prop/for-all [[sf-player foac-player] straight-flush-vs-four-of-a-kind-gen]
                (= "White wins. with straight flush" (winner sf-player foac-player))))

(def four-of-a-kind-cards-gen-2
  (gen/fmap (fn [deck]
              (let [quadruplet (rand-nth (partition-by first (sort-by (comp rank->ordinal first) deck)))
                    other-card (take 1 (filter (complement (apply hash-set quadruplet)) deck))]
                (shuffle (into (apply vector quadruplet) other-card))))
            deck-gen))

;;; An example of a parameterized generator
;;;
;;; The idea here would be to use this when you needed to reference
;;; the same deck. I'm trying to figure out how to avoid gen/such-that
(defn four-of-a-kind-cards-gen-3
  ([]
   (four-of-a-kind-cards-gen-3 1))
  ([num-hands]
   (gen/fmap (fn [deck]
               (let [quadruplets (repeatedly num-hands (partial rand-nth (partition-by first (sort-by (comp rank->ordinal first) deck))))
                     other-cards (take num-hands (filter (complement (reduce into #{} quadruplets)) deck))]
                 (mapv (fn [quadruplet card]
                         (into (apply vector quadruplet) [card]))
                       quadruplets
                       other-cards)))
             deck-gen)))

;;; Let's try to make some generators that depend on shared deck state

(defn deck-gen-2 [removed-cards-set]
  (gen/fmap (fn [deck]
              (filter (complement removed-cards-set) deck))
            (gen/shuffle [[2 \C] [3 \C] [4 \C] [5 \C] [6 \C] [7 \C] [8 \C] [9 \C] [10 \C] [\J \C] [\Q \C] [\K \C] [\A \C] [2 \D] [3 \D] [4 \D] [5 \D] [6 \D] [7 \D] [8 \D] [9 \D] [10 \D] [\J \D] [\Q \D] [\K \D] [\A \D] [2 \H] [3 \H] [4 \H] [5 \H] [6 \H] [7 \H] [8 \H] [9 \H] [10 \H] [\J \H] [\Q \H] [\K \H] [\A \H] [2 \S] [3 \S] [4 \S] [5 \S] [6 \S] [7 \S] [8 \S] [9 \S] [10 \S] [\J \S] [\Q \S] [\K \S] [\A \S]])))

(defspec test-deck-gen-2-count
  (prop/for-all [deck (deck-gen-2 #{})]
                (= 52 (count deck))))

(defn straight-flush-from-deck [deck]
  (->> deck
       (sort-by (juxt second (comp rank->ordinal first)))
       (partition-by second)
       rand-nth
       (partition-all 5 1)
       (filter (fn [part] (= 5 (count part))))
       rand-nth))

(defn straight-flush-gen-2 [the-deck-gen]
  (gen/bind the-deck-gen
            (fn [deck]
              (gen/return (straight-flush-from-deck deck)))))

(defn quadruplet-from-deck [deck]
   (->> deck
        (sort-by (comp rank->ordinal first))
        (partition-by first)
        (filter (fn [part] (= 4 (count part))))
        rand-nth))

(defn cards-from-deck [n deck]
  (take n deck))

(defn remove-cards [cards deck]
  (filter (complement (apply hash-set cards)) deck))

(defn four-of-a-kind-from-deck [deck]
  (let [quadruplet (quadruplet-from-deck deck)
        other-card (cards-from-deck 1 (remove-cards quadruplet deck))]
    (shuffle (into quadruplet other-card))))

(defn four-of-a-kind-gen [the-deck-gen]
  (gen/bind the-deck-gen
            (fn [deck]
              (gen/return (four-of-a-kind-from-deck deck)))))

(defspec test-four-of-a-kind-gen-2
  (prop/for-all [four-of-a-kind (four-of-a-kind-gen (deck-gen-2 #{}))]
                (= four-of-a-kind? (hand four-of-a-kind))))

(defn triplet-from-deck [deck]
  (->> deck
       (sort-by (comp rank->ordinal first))
       (partition-by first)
       (filter (fn [part] (<= 3 (count part))))
       rand-nth
       (take 3)))

(defn pair-from-deck [deck]
  (->> deck
       (sort-by (comp rank->ordinal first))
       (partition-by first)
       (filter (fn [part] (<= 2 (count part))))
       rand-nth
       (take 2)))

(defn full-house-from-deck [deck]
  (let [triplet (triplet-from-deck deck)
        pair (pair-from-deck (remove-cards triplet deck))]
    (shuffle (into triplet pair))))

(defn full-house-gen [the-deck-gen]
  (gen/bind the-deck-gen
            (fn [deck]
              (gen/return (full-house-from-deck deck)))))

(defspec test-full-house-gen
  (prop/for-all [full-house (full-house-gen (deck-gen-2 #{}))]
                (= full-house? (hand full-house))))

(defspec test-full-house-gen-count
  (prop/for-all [full-house (full-house-gen (deck-gen-2 #{}))]
                (= 5 (count full-house))))

(defn game-gen-3 [white-gens black-gens]
  (gen/fmap (fn [[w-cards b-cards]]
              [{:color "White"
                :cards w-cards}
               {:color "Black"
                :cards b-cards}])
            (gen/bind ((rand-nth white-gens) (deck-gen-2 #{}))
                      (fn [hand]
                        (let [deck-gen (deck-gen-2 (apply hash-set hand))
                              black-gen ((rand-nth black-gens) deck-gen)
                              _ (def charnock black-gen)
                              _ (def whitefield deck-gen)]
                          (gen/tuple (gen/return hand)
                                     black-gen))))))

;;; fails: black is generated with 4 cards in the hand, one of which
;;; is always [2 \C]
;;;
;;; I take that back. It's only very often there. I've seen one run
;;; with the [4 \C]
(defspec test-game-gen-3-valid-hands
  (prop/for-all [[{w-cards :cards} {b-cards :cards}] (game-gen-3 [straight-flush-gen-2]
                                                                 [four-of-a-kind-gen full-house-gen])]
                (= 5 (count w-cards) (count b-cards))))

(defspec test-straight-flush-beats-all
  (prop/for-all [game (game-gen-3 [straight-flush-gen-2]
                                  [four-of-a-kind-gen full-house-gen])]
                (= "White wins. with straight flush" (apply winner game))))
