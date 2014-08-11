(ns ironcast.internals.input-test
  (:require [clojure.test :refer :all]
            [ironcast.internals.input :refer :all]))

(def reverse-map
  (into {}
        (map #(vector (second %) (first %))
             (concat key-map button-map))))

(defn input
  [m]
  (reify IInput
    (get-x [this] (:x m 0))
    (get-y [this] (:y m 0))
    (button-pressed? [this button] (get (:pressed m) (reverse-map button)))
    (key-pressed? [this key] (get (:pressed m) (reverse-map key)))))


(deftest test-hit
  (testing "A key is hit when a key is pressed in the previous state, and no-longer in the current state (released)"
    (let [prev (input {:pressed #{:left :w}})
          current (input {:pressed #{:w}})]
      (testing "In this instance, 'left' should be hit"
        (is (= (set (hit (get-state prev) current)) #{:left}))))))

(deftest test-commands
  (testing
      "A command can be hit in one of 2 states, hold or press.
      Press is a synonym to 'hit' and hold is a synonym for 'pressed or down' in terms of the actual key"
    (let [commands {:do-foo (hold :w)
                    :do-baz (press :right)
                    :do-bar (press :right)}]
      (let [state {:pressed #{:right}
                   :hit #{:w}}]
        (testing "In this state, neither do-foo or do-bar should be hit"
          (is (= #{} (set (commands-hit state commands))))))
      (let [state {:pressed #{:w :right}}]
        (testing "In this state, 'do-foo' should be hit"
          (is (= #{:do-foo} (set (commands-hit state commands))))))
      (let [state {:hit #{:right :w}}]
        (testing "In this state 'do-bar' and 'do-baz' should be hit"
          (is (= #{:do-bar :do-baz} (set (commands-hit state commands)))))))))
