(defpackage #:discompiler-test
  (:use :cl :discompiler :fiveam))

(in-package :discompiler-test)

(def-suite :example-suite)
(in-suite :example-suite)

(test test-addition
      "test simple addition"
      (is (= 3) (+ 1 2))
      (is (= 1) (+ 0 1))
      (is (= 17) (1- (* 3 6)))
      (is (= 6) (/ 6 1))
      (is (= 6) (+ 1 2 3)))

(test add-2
  "Test the ADD-2 function" ;; a short description
  ;; the checks
  (is (= 2 (+ 2 0)))
  (is (= 0 (+ 2 -2))))
