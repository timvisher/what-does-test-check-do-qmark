(ns what-does-test-check-do-qmark.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]

            [what-does-test-check-do-qmark.core :refer :all]))

(def rank-gen (gen/elements [1 2 3 4 5 6 7 8 9 10 \J \Q \K \A]))

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

(def deck-gen (gen/shuffle [[1 \C] [2 \C] [3 \C] [4 \C] [5 \C] [6 \C] [7 \C] [8 \C] [9 \C] [10 \C] [\J \C] [\Q \C] [\K \C] [\A \C] [1 \D] [2 \D] [3 \D] [4 \D] [5 \D] [6 \D] [7 \D] [8 \D] [9 \D] [10 \D] [\J \D] [\Q \D] [\K \D] [\A \D] [1 \H] [2 \H] [3 \H] [4 \H] [5 \H] [6 \H] [7 \H] [8 \H] [9 \H] [10 \H] [\J \H] [\Q \H] [\K \H] [\A \H] [1 \S] [2 \S] [3 \S] [4 \S] [5 \S] [6 \S] [7 \S] [8 \S] [9 \S] [10 \S] [\J \S] [\Q \S] [\K \S] [\A \S]]))

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

(defn random-consecutive-cards []
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

(def consecutive-cards-gen
  (gen/return (random-consecutive-cards)))
