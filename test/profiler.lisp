(in-package :discompiler)

(require :sb-sprof)
(declaim (optimize speed))

;;;; CPU profiling test1
(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop nil)
  (make-instance 'memory :start 1 :end 100))

(sb-sprof:profile-call-counts "discompiler")

;;; test2
(sb-sprof:with-profiling (:max-samples 1000
                                       :report :flat
                                       :loop t
                                       :show-progress t)
  (+ 1 (+ 1 1) (+ 1 1 1)))

;;;; Allocation profiling
(sb-sprof:start-profiling)
(sb-sprof:with-profiling (
                          :mode :alloc
                          :report :flat
                          :show-progress nil
                          :loop T)
  (fiveam:run :memory))
(sb-sprof:stop-profiling)
(sb-sprof:report)
