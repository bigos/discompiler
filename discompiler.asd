
(defsystem #:discompiler
  :serial t
  :version "0.0.1"
  :author "Jacek Podkanski"
  :licence "GPLv3"
  :depends-on (:lisp-unit :cl-ppcre :cl-fad :cl-utilities :md5)
  :components ((:file "packages") 	  
               (:file "constants")
               (:module "src"
                        :components (
                                     (:file "pe-header")
                                     (:file "coff-header")
                                     (:file "optional-header")

                                     (:file "byte-utilities")
                                     (:file "reference-data")
                                     (:file "summary-table-columns")))
               (:module "test"
                        :components ((:file "lisp-unit")
                                     (:file "tests")))
               (:file "discompiler"))
  :description "machine code experiment"
  :long-description "experimenting with Intel machine code")
