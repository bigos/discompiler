;(in-package :discompiler)

(asdf:defsystem #:discompiler

  :version "0.0.2"
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :depends-on (:lisp-unit :cl-ppcre :cl-fad :cl-utilities :md5)
  :components (
               (:file "packages")
               (:file "constants" :depends-on ("discompiler"))
               (:module "src"
                        :depends-on ("discompiler")
                        :components
                        ((:module "pecoff-executable"
                                        ;:depends-on ("src")
                                  :components
                                  (
                                   (:file "pe-header")
                                   (:file "coff-header")
                                   (:file "optional-header")))
                         (:file "section-table")
                         (:file "byte-utilities")
                         (:file "reference-data")
                         (:file "summary-table-columns")
                         (:file "header-info")
                         (:file "loader")))
               (:module "test"
                        :depends-on ("discompiler")
                        :components ((:file "lisp-unit")
                                     (:file "tests")))
               (:file "discompiler" :depends-on ("packages")))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code")
