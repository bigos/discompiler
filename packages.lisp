(defpackage #:discompiler
  (:use :cl :lisp-unit :md5)
  (:export #:file-to-bytes
           #:loader
           #:imported-functions))

(defpackage #:reference-data
  (:use :cl)
  (:export :process-reference-files))
