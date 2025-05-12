(defpackage :oscl.test.util
  (:use :cl :rove :oscl))
(in-package :oscl.test.util)

(deftest parse-osc-str-test
  (let* ((buf (make-array 16 :element-type '(unsigned-byte 8)
                          :initial-contents #(47 116 101 115 116 0 0 0 0 0 0 0 0 0 0 0))) ; "/test\0\0\0"
         (str nil)
         (next nil))
    (multiple-value-setq (str next) (parse-osc-str buf 0))
    (ok (string= str "/test"))
    (ok (= next 8))))

(deftest parse-osc-int-test
  (let* ((buf (make-array 4 :element-type '(unsigned-byte 8)
                          :initial-contents #(0 0 1 44))) ; = 300 (0x012C)
         (val nil)
         (next nil))
    (multiple-value-setq (val next) (parse-osc-int buf 0))
    (ok (= val 300))
    (ok (= next 4))))

(deftest parse-osc-float-test
  (let* ((float-val 3.14)
         (int-val (sb-kernel:single-float-bits float-val))
         (buf (make-array 4 :element-type '(unsigned-byte 8)
                          :initial-contents (list (ldb (byte 8 24) int-val)
                                                  (ldb (byte 8 16) int-val)
                                                  (ldb (byte 8 8) int-val)
                                                  (ldb (byte 8 0) int-val))))
         (parsed nil)
         (next nil))
    (multiple-value-setq (parsed next) (parse-osc-float buf 0))
    (ok (< (abs (- parsed 3.14)) 0.0001))
    (ok (= next 4))))