(in-package :discompiler)

;;; (require :sb-sprof)
(declaim (optimize speed))


(defun own-profiling ()
  ;; Allocation profiling
  (sb-sprof:start-profiling)
  (sb-sprof:with-profiling (
                            :mode :alloc
                            :report :flat
                            :show-progress nil
                            :loop T)
    (fiveam:run :memory))
  (sb-sprof:stop-profiling)
  (sb-sprof:report))
