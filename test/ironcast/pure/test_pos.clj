(ns ironcast.pure.test-pos
  (:require [clojure.test :refer :all]
            [ironcast.pure
             [pos :refer :all]
             [attr :refer :all]]))


(deftest test-pos
  (testing "I can place an entity at a position"
    (let [with-ent (put {} 0 [4 4])]
      (is (= (pos with-ent 0) [4 4]))
      (testing "If I place another entity, both positions are still valid"
        (let [with-ents (put with-ent 1 [5 6])]
          (is (= (pos with-ents 0) [4 4]))
          (is (= (pos with-ents 1) [5 6]))))
      (testing "I can place another entity at the same position... both entities can share the position"
        (let [with-ents (put with-ent 1 [4 4])]
          (is (= (pos with-ents 0) [4 4]))
          (is (= (pos with-ents 1) [4 4]))
          (is (= (at with-ents [4 4]) #{0 1}))))
      (testing "An entity however, can only occupy one position at a time"
        (let [with-ent (put with-ent 0 [5 5])]
          (is (= (pos with-ent 0) [5 5]))
          (is (= (at with-ent [5 5]) #{0}))
          (is (= (at with-ent [4 4]) #{}))))
      (testing "Moving an entity to a position that is occupied by a solid entity is not possible."
        (let [with-obstacle (-> (solidify with-ent 1)
                                (put 1 [5 5]))]
          (is (= [with-obstacle false] (try-put with-obstacle 0 [5 5])))
          (is (= with-obstacle (put with-obstacle 0 [5 5]))))))))