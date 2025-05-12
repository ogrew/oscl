(defpackage :oscl.test.builder
  (:use :cl :rove :oscl))
(in-package :oscl.test.builder)

(deftest build-osc-str-test
  (testing "build-osc-str with no padding needed"
    (let ((result (oscl:build-osc-str "abc")))
      (ok (= (length result) 4))
      (ok (equalp result #(97 98 99 0)))))
  (testing "build-osc-str with 3-byte padding"
    (let ((result (oscl:build-osc-str "a")))
      (ok (= (length result) 4)) ; "a" + null + 2 padding = 3 → padded to 4
      (ok (equalp result #(97 0 0 0)))))
  (testing "build-osc-str with 2-byte padding"
    (let ((result (oscl:build-osc-str "ab")))
      (ok (= (length result) 4)) ; "ab" + null = 3 → padded to 4, total 4
      (ok (equalp result #(97 98 0 0))))))

(deftest build-osc-int-test
  (testing "build-osc-int for integer 42"
    (ok (equalp (build-osc-int 42) #(0 0 0 42))))
  (testing "build-osc-int for big-endian boundary check"
    (ok (equalp (build-osc-int 16909060) #(1 2 3 4))))) ; 0x01020304

(deftest build-osc-float-test
  (testing "build-osc-float for 0.0"
    (ok (equalp (build-osc-float 0.0) #(0 0 0 0))))
  (testing "build-osc-float for 1.0"
    (ok (equalp (build-osc-float 1.0) #(63 128 0 0))))) ; IEEE 754: 0x3f800000

(deftest osc-type-tag-test
  (testing "type tag for integer"
    (ok (char= (osc-type-tag 123) #\i)))
  (testing "type tag for float"
    (ok (char= (osc-type-tag 1.0) #\f)))
  (testing "type tag for string"
    (ok (char= (osc-type-tag "hello") #\s)))
  (testing "type tag error for unsupported type"
    (ok (signals (osc-type-tag '(1 2 3)) 'error))))