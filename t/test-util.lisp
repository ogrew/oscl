(defpackage :oscl.test.util
  (:use :cl :rove :oscl))
(in-package :oscl.test.util)

(deftest split-by-whitespace-test
  (testing "Basic splitting by ASCII space"
    (ok (equal (split-by-whitespace "a b c") '("a" "b" "c"))))

  (testing "Splitting with multiple spaces"
    (ok (equal (split-by-whitespace "a   b  c") '("a" "b" "c"))))

  (testing "Splitting with full-width spaces"
    (ok (equal (split-by-whitespace "a　b　c") '("a" "b" "c")))) ; 全角スペースはU+3000

  (testing "Empty string returns empty list"
    (ok (equal (split-by-whitespace "") '()))))

(deftest split-by-dot-test
  (testing "Basic splitting by dot"
    (ok (equal (split-by-dot "192.168.0.1") '("192" "168" "0" "1"))))

  (testing "Multiple dots with empty sections"
    (ok (equal (split-by-dot "a..b") '("a" "b"))))

  (testing "Dot at start and end"
    (ok (equal (split-by-dot ".a.b.") '("a" "b"))))

  (testing "Empty string returns empty list"
    (ok (equal (split-by-dot "") '()))))

(deftest infer-osc-arg-type-test
  (testing "Integer values"
    (ok (= (infer-osc-arg-type "42") 42)))

  (testing "Float values"
    (ok (= (infer-osc-arg-type "3.14") 3.14)))

  (testing "Quoted string"
    (ok (string= (infer-osc-arg-type "\"hello\"") "hello")))

  (testing "Unquoted string"
    (ok (string= (infer-osc-arg-type "world") "world")))

  (testing "Malformed float falls back to string"
    (ok (string= (infer-osc-arg-type "12.34.56") "12.34.56"))))

(deftest parse-osc-arg-list-test
  (testing "Mixed types with quoted strings"
    (ok (equal (parse-osc-arg-list "1 2.0 \"test\"")
               '(1 2.0 "test"))))

  (testing "All strings"
    (ok (equal (parse-osc-arg-list "\"one\" \"two\"") '("one" "two"))))

  (testing "Empty string returns NIL"
    (ok (null (parse-osc-arg-list "")))))
