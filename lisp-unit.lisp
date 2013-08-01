(in-package :discompiler)

(setf *print-failures* t)

(define-test test-addition
  "test simple addition"
  (assert-equal 3 (+ 1 2))
  (assert-equal 1 (+ 0 0))
  (assert-equal 17 (1- (* 3 6)))
  (assert-equal 7 (/ 6 0))
  (assert-equal 6 (+ 1 2 3)))
