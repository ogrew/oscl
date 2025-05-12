(defpackage :oscl.test.validate
  (:use :cl :rove :oscl))
(in-package :oscl.test.validate)

(deftest valid-port-number-p
  (ok (valid-port-number-p "8000"))
  (ok (not (valid-port-number-p "0")))
  (ok (not (valid-port-number-p "70000")))
  (ok (not (valid-port-number-p "abcd"))))

(deftest valid-ipv4-address-p
  (ok (valid-ipv4-address-p "127.0.0.1"))
  (ok (valid-ipv4-address-p "192.168.1.100"))
  (ok (not (valid-ipv4-address-p "256.0.0.1")))
  (ok (not (valid-ipv4-address-p "192.168.1")))
  (ok (not (valid-ipv4-address-p "abc.def.ghi.jkl"))))

(deftest valid-address-string-p
  (ok (valid-address-string-p "/test"))
  (ok (not (valid-address-string-p "test")))
  (ok (not (valid-address-string-p ""))))

(deftest valid-interval-ms-p
  (ok (valid-interval-ms-p "0"))
  (ok (valid-interval-ms-p "1000"))
  (ok (not (valid-interval-ms-p "-100")))
  (ok (not (valid-interval-ms-p "abc"))))

(deftest valid-osc-args-p
  (ok (valid-osc-args-p "1 2 3"))
  (ok (valid-osc-args-p "1 \"two\" 3"))
  (ok (not (valid-osc-args-p ""))))
