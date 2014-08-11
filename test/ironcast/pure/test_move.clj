(ns ironcast.pure.test-move
  (:require [ironcast.pure
             [attr :refer :all]
             [pos :refer :all]
             [move :refer :all]]
            [clojure.test :refer :all]
            [ironcast.pure.time :as time]))

(def grid {:width 50 :height 50})

(deftest basic-movement
  (testing "if I put an entity on grid I can..."
    (let [with-goal (-> grid
                        (put 0 [3 4])
                        (move 0 [5 5]))]
      (testing "assign it a goal"
        (is (= (goal with-goal 0)
               [5 5])))
      (testing "see if I should move"
        (is (should-path? with-goal 0)))
      (testing "derive a path"
        (is (= (make-path with-goal 0)
               [[4 4] [5 5]] )))
      (testing "add that path"
        (let [with-path (add-path with-goal 0 (make-path with-goal 0))]
          (is (= (path with-path 0)
                 [[4 4] [5 5]]))
          (testing "I should no longer want to move"
            (is (not (should-path? with-path 0))))
          (testing "I should then be able to move via step"
            (let [with-move (step with-path 0)]
              (is (= (pos with-move 0) [4 4]))
              (is (= (path with-move 0) [[5 5]]))))
          (testing "if I put a solid entity in the way..."
            (let [with-obstacle (-> with-path
                                    (solidify 1)
                                    (put 1 [4 4]))]
              (testing "I should not move to [4 4]"
                (let [with-move (step-all with-obstacle)]
                  (is (= (pos with-move 0) [3 4]))
                  (testing "I should be stuck"
                    (is (stuck? with-move 0)))
                  (testing "therefore I should try to move again using a different path"
                    (is (should-path? with-move 0)))
                  (testing "the new path should be different to the old"
                    (is (= (make-path with-move 0)
                              [[4 5] [5 5]])))
                  (testing "but if I move it out of the way"
                    (let [outta-way (-> with-obstacle (put 1 [6 6]))]
                      (testing "I should derive the old path again"
                        (is (= (make-path outta-way 0)
                               [[4 4] [5 5]])))))
                  (testing "If I follow the new path I will no longer be stuck"
                    (is (not (stuck? (step-all (make-path with-move 0)) 0))))))))
          (testing "if I put a solid entity at the goal and make my step to [4 4]"
            (let [with-obstacle (-> with-path
                                    (solidify 1)
                                    (put 1 [5 5])
                                    (step 0))]
              (is (= (pos with-obstacle 0) [4 4]))
              (testing "moving to [5 5] will fail"
                (let [with-move (step-all with-obstacle)]
                  (is (has-flag? with-move 0 :stuck))
                  (is (= (pos with-move 0) [4 4]))
                  (testing "I should probably re-goal"
                    (is (should-re-goal? with-move 0))
                    (testing "lets pick a new goal"
                      (is (= (pos (re-goal with-move 0) 0) [4 4])))))))))))))

