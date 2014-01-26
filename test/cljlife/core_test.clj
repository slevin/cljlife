(ns cljlife.core-test
  (:require [clojure.test :refer :all]
            [cljlife.core :refer :all]))

(def o \space)
(def x \*)
(def ooo [o o o])
(def xoo [x o o])
(def oxo [o x o])
(def oox [o o x])
(def xxo [x x o])
(def xox [x o x])
(def oxx [o x x])
(def xxx [x x x])

(deftest neighbors-test
  (testing "finds neighbors"
    (is (= (neighbors 1 1 [xox oxo ooo]) [x o x o o o o o])))
  (testing "filters live neighbors"
    (is (= (live-neighbors (neighbors 1 1 [xox xxx xxo])) [x x x x x x]))))

(deftest border-neighbors-test
  (testing "find topleft border neighbors"
    (is (= (neighbors 0 0 [xxo xxo ooo]) [o o o o x o x x])))
  (testing "find bottom right border neighbors"
    (is (= (neighbors 2 2 [ooo oxx oxx]) [x x o x o o o o]))))

(deftest nothing-test
  (testing "nothing is the same"
    (is (= (next-state [ooo ooo ooo]) [ooo ooo ooo]))))

(deftest live-to-nothing-test
  (testing "0 neighbors, dies"
    (is (= (next-item 1 1 [ooo oxo ooo]) o)))
  (testing "1 neighbors, dies"
    (is (= (next-item 1 1 [oxo oxo ooo]) o)))
  (testing "4 neighbors, dies"
    (is (= (next-item 1 1 [oxo oxo xxx]) o)))
  (testing "5 neighbors, dies"
    (is (= (next-item 1 1 [xxo oxo xxx]) o)))
  (testing "6 neighbors, dies"
    (is (= (next-item 1 1 [xxx oxo xxx]) o)))
  (testing "7 neighbors, dies"
    (is (= (next-item 1 1 [xxx xxo xxx]) o)))
  (testing "8 neighbors, dies"
    (is (= (next-item 1 1 [xxx xxx xxx]) o))))

(deftest live-to-live-test
  (testing "2 neighbors, alive"
    (is (= (next-item 1 1 [oxo oxo oxo]) x)))
  (testing "3 neighbors, alive"
    (is (= (next-item 1 1 [xxo oxo oxo]) x))))

(deftest nothing-to-live-test
  (testing "3 neghbors born"
    (is (= (next-item 1 1 [xxx ooo ooo]) x))))


(deftest full-state-test
  (testing "still life block"
    (is (= (next-state [xxo xxo ooo]) [xxo xxo ooo])))
  (testing "blinker block"
    (is (= (next-state [oxo oxo oxo]) [ooo xxx ooo])))
  (testing "blinker block 2"
    (is (= (next-state [ooo xxx ooo]) [oxo oxo oxo]))))

(deftest print-test
  (testing "creates valid ascii representation"
    (is (= (state-string [oxo oxo xox]) " * \n * \n* *"))))

(deftest fail-test
  (is (= 1 0)))

; test to reject invalid shapes
