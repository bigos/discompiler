(defpackage #:discompiler-asd
  (:use :cl :asdf))

(in-package :discompiler-asd)

(defsystem #:discompiler
  :version "0.0.2"
  :serial t
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :depends-on (:lisp-unit :cl-ppcre :cl-fad :cl-utilities :md5)
  :components ((:file "packages")
               (:file "discompiler")
               (:module "src"
                        :components
                        ((:file "constants" )
                         (:file "byte-utilities")
                         (:module "pecoff-executable"
                                  :components
                                  ((:file "header-info")
                                   (:file "memory")
                                   (:file "loader")
                                   (:file "imports")
                                   (:file "exports")
                                   (:module "file-structure"
                                            :components
                                            ((:file "pe-header")
                                             (:file "optional-header")
                                             (:file "section-table")
                                             (:file "coff-header")))))
                         (:module "reference"
                                  :components
                                  ((:file "reference-data")
                                   (:file "summary-table-columns")))))
               (:module "test"
                        :components
                        ((:file "lisp-unit")
                         (:file "tests"))))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code")
