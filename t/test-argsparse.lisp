(defpackage :oscl.test.argsparse
  (:use :cl :rove :oscl))
(in-package :oscl.test.argsparse)

(deftest parse-send-args-test
  (multiple-value-bind (host port address interval-ms args file loop-flag)
      (parse-send-args '("--host" "localhost" "--port" "9000" "--address" "/test"
                         "--interval" "1000" "--args" "1 2.0 hello"))
    (ok (string= host "127.0.0.1"))
    (ok (= port 9000))
    (ok (string= address "/test"))
    (ok (= interval-ms 1000))
    (ok (equal args '(1 2.0 "hello")))
    (ok (null file))
    (ok (not loop-flag))))

(deftest parse-send-args-loop-flag-test
  (multiple-value-bind (host port address interval-ms args file loop-flag)
      (parse-send-args '("--from" "t/test-data/valid01.json" "--loop"))
    (ok (string= file "t/test-data/valid01.json"))
    (ok loop-flag)))

(deftest parse-recv-args-test
  (multiple-value-bind (port filter mode raw)
      (parse-recv-args '("--port" "7000" "--filter" "test" "--raw"))
    (ok (= port 7000))
    (ok (string= filter "test"))
    (ok (eq mode :include))
    (ok raw)))

(deftest parse-recv-args-exclude-test
  (multiple-value-bind (port filter mode raw)
      (parse-recv-args '("--filter" "-debug"))
    (ok (= port 9000)) ;; default
    (ok (string= filter "debug"))
    (ok (eq mode :exclude))
    (ok (null raw))))

(deftest parse-bridge-args-test
  (multiple-value-bind (in-host in-port out-host out-port filter mode)
      (parse-bridge-args '("--in-host" "localhost" "--in-port" "9001"
                           "--out-host" "127.0.0.1" "--out-port" "9010"
                           "--filter" "test"))
    (ok (string= in-host "127.0.0.1"))   ; localhost → 127.0.0.1 に変換される仕様
    (ok (= in-port 9001))
    (ok (string= out-host "127.0.0.1"))
    (ok (= out-port 9010))
    (ok (string= filter "test"))
    (ok (eq mode :include))))
