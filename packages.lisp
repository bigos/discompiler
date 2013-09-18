(defpackage #:discompiler
  (:use :cl :asdf :lisp-unit :md5)
  (:export :run))

(defpackage #:reference-data
  (:use :cl)
  (:export :process-reference-files))
