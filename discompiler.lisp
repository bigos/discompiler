(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  (test-arithmetic)
  (test-mod-vals))

(defun int-to-bin (int) 
  (format nil "~8,'0B" int))
