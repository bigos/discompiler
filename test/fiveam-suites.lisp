(defpackage #:discompiler-test
  (:use :cl :discompiler :fiveam)
  (:shadowing-import-from :discompiler :run)
  )

(in-package :discompiler-test)

(def-suite :all)
(def-suite :pe-coff :in :all)
(def-suite :elf :in :all)
