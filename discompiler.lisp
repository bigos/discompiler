(in-package :discompiler)


(lisp-unit:define-test test-addition
    "test simple addition"
  (lisp-unit:assert-equal 3 (+ 1 2))
  (lisp-unit:assert-equal 1 (+ 0 0)))

(defun run ()
  (format t "running skeleton program")

  (setf lisp-unit:*print-failures* t)
  (lisp-unit:run-tests :all :discompiler)
  )

